{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module: ChainwebNode
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Main
(
-- * Configuration
  ChainwebNodeConfiguration(..)

-- * Monitor
, runCutMonitor
, runRtsMonitor

-- * Chainweb Node
, node
, withNodeLogger

-- * Main function
, main
) where

import Configuration.Utils hiding (Error)
import Configuration.Utils.Validation (validateFilePath)

import Control.Concurrent
import Control.Concurrent.Async
import Control.DeepSeq
import Control.Exception
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.Managed
import Control.Monad.Trans.Resource (runResourceT)

import Data.Maybe
import Data.Text (Text)
import Data.Typeable

import GHC.Generics hiding (from)
import GHC.Stack
import GHC.Stats

import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTPS

import System.Directory
import System.FilePath
import System.IO
import qualified System.Logger as L
import System.LogLevel
import System.Mem

-- internal modules

import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB.PruneForks (PruneStats(..))
import Chainweb.Chainweb
import Chainweb.Chainweb.Configuration
import Chainweb.Chainweb.CutResources
import Chainweb.Counter
import Chainweb.Cut.CutHashes
import Chainweb.CutDB
import Chainweb.Logger
import Chainweb.Logging.Config
import Chainweb.Logging.Miner
import Chainweb.Pact.Mempool.InMemTypes (MempoolStats(..))
import Chainweb.Pact.Backend.DbCache (DbCacheStats)
import Chainweb.Pact.RestAPI.Server (PactCmdLog(..))
import Chainweb.Pact.Types
import Chainweb.Scheduler (Scheduler, scheduleEvery, withScheduler)
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Utils.RequestLog
import Chainweb.Version
import Chainweb.Version.Mainnet

import Chainweb.Storage.Table.RocksDB
import Data.LogMessage
import P2P.Node
import PkgInfo
import Utils.CheckRLimits
import Utils.InstallSignalHandlers
import Utils.Logging
import Utils.Logging.Config
import Utils.Logging.Trace

-- -------------------------------------------------------------------------- --
-- Configuration

data ChainwebNodeConfiguration = ChainwebNodeConfiguration
    { _nodeConfigChainweb :: !ChainwebConfiguration
    , _nodeConfigLog :: !LogConfig
    , _nodeConfigDatabaseDirectory :: !(Maybe FilePath)
    }
    deriving (Show, Eq, Generic)

makeLenses ''ChainwebNodeConfiguration

defaultChainwebNodeConfiguration :: ChainwebNodeConfiguration
defaultChainwebNodeConfiguration = ChainwebNodeConfiguration
    { _nodeConfigChainweb = defaultChainwebConfiguration Mainnet01
    , _nodeConfigLog = defaultLogConfig
        & logConfigLogger . L.loggerConfigThreshold .~ level
    , _nodeConfigDatabaseDirectory = Nothing
    }
  where
    level = L.Info

validateChainwebNodeConfiguration :: ConfigValidation ChainwebNodeConfiguration []
validateChainwebNodeConfiguration o = do
    validateLogConfig $ _nodeConfigLog o
    validateChainwebConfiguration $ _nodeConfigChainweb o
    mapM_ (validateFilePath "databaseDirectory") (_nodeConfigDatabaseDirectory o)

instance ToJSON ChainwebNodeConfiguration where
    toJSON o = object
        [ "chainweb" .= _nodeConfigChainweb o
        , "logging" .= _nodeConfigLog o
        , "databaseDirectory" .= _nodeConfigDatabaseDirectory o
        ]

instance FromJSON (ChainwebNodeConfiguration -> ChainwebNodeConfiguration) where
    parseJSON = withObject "ChainwebNodeConfig" $ \o -> id
        <$< nodeConfigChainweb %.: "chainweb" % o
        <*< nodeConfigLog %.: "logging" % o
        <*< nodeConfigDatabaseDirectory ..: "databaseDirectory" % o

pChainwebNodeConfiguration :: MParser ChainwebNodeConfiguration
pChainwebNodeConfiguration = id
    <$< nodeConfigChainweb %:: pChainwebConfiguration
    <*< parserOptionGroup "Logging" (nodeConfigLog %:: pLogConfig)
    <*< nodeConfigDatabaseDirectory .:: fmap Just % textOption
        % long "database-directory"
        <> help "directory where the databases are persisted"

getRocksDbDir :: HasCallStack => ChainwebNodeConfiguration -> IO FilePath
getRocksDbDir conf = (\base -> base </> "0" </> "rocksDb") <$> getDbBaseDir conf

getPactDbDir :: HasCallStack => ChainwebNodeConfiguration -> IO FilePath
getPactDbDir conf =  (\base -> base </> "0" </> "sqlite")  <$> getDbBaseDir conf

getBackupsDir :: HasCallStack => ChainwebNodeConfiguration -> IO FilePath
getBackupsDir conf = (</> "backups") <$> getDbBaseDir conf

getDbBaseDir :: HasCallStack => ChainwebNodeConfiguration -> IO FilePath
getDbBaseDir conf = case _nodeConfigDatabaseDirectory conf of
    Nothing -> getXdgDirectory XdgData
        $ "chainweb-node" </> sshow (_versionName v)
    Just d -> return d
  where
    v = _configChainwebVersion $ _nodeConfigChainweb conf

-- -------------------------------------------------------------------------- --
-- Monitors

-- | Schedule a monitor action on the shared background job scheduler. The
-- first run happens one interval after the node started.
--
runCutMonitor
    :: HasVersion
    => Logger logger
    => Scheduler
    -> logger
    -> CutDb logger
    -> IO ()
runCutMonitor sched logger db = L.withLoggerLabel ("component", "cut-monitor") logger $ \l ->
    scheduleEvery sched "ChainwebNode.runCutMonitor" 15_000_000 $ do
        logFunctionJson l Info . cutToCutHashes Nothing
            =<< _cut db

data BlockUpdate = BlockUpdate
    { _blockUpdateBlockHeader :: !(ObjectEncoded BlockHeader)
    , _blockUpdateOrphaned :: !Bool
    , _blockUpdateTxCount :: !Int
    , _blockUpdateDifficultyDouble :: !Double
    }
    deriving (Show, Eq, Ord, Generic, NFData)

instance HasVersion => ToJSON BlockUpdate where
    toEncoding o = pairs
        $ "header" .= _blockUpdateBlockHeader o
        <> "orphaned" .= _blockUpdateOrphaned o
        <> "txCount" .= _blockUpdateTxCount o
        <> "difficultyDouble" .= _blockUpdateDifficultyDouble o
    toJSON o = object
        [ "header" .= _blockUpdateBlockHeader o
        , "orphaned" .= _blockUpdateOrphaned o
        , "txCount" .= _blockUpdateTxCount o
        , "difficultyDouble" .= _blockUpdateDifficultyDouble o
        ]

    {-# INLINE toEncoding #-}
    {-# INLINE toJSON #-}

-- runBlockUpdateMonitor :: Logger logger => logger -> CutDb -> IO ()
-- runBlockUpdateMonitor logger db = L.withLoggerLabel ("component", "block-update-monitor") logger $ \l ->
--     runMonitorLoop "ChainwebNode.runBlockUpdateMonitor" l $ do
--         blockDiffStream db
--             & S.mapM toUpdate
--             & S.mapM_ (logFunctionJson l Info)
--   where
--     toUpdate :: Either BlockHeader BlockHeader -> IO BlockUpdate
--     toUpdate (Right bh) = BlockUpdate
--         <$> pure (ObjectEncoded bh) -- _blockUpdateBlockHeader
--         <*> pure False -- _blockUpdateOrphaned
--         <*> txCount bh -- _blockUpdateTxCount
--         <*> pure (difficultyToDouble (targetToDifficulty (view blockTarget bh))) -- _blockUpdateDifficultyDouble
--     toUpdate (Left bh) = BlockUpdate
--         <$> pure (ObjectEncoded bh) -- _blockUpdateBlockHeader
--         <*> pure True -- _blockUpdateOrphaned
--         <*> ((0 -) <$> txCount bh) -- _blockUpdateTxCount
--         <*> pure (difficultyToDouble (targetToDifficulty (view blockTarget bh))) -- _blockUpdateDifficultyDouble

-- type CutLog = HM.HashMap ChainId (ObjectEncoded BlockHeader)

-- This instances are OK, since this is the "Main" module of an application
--
deriving instance NFData GCDetails
deriving instance NFData RTSStats

deriving instance ToJSON GCDetails
deriving instance ToJSON RTSStats

runRtsMonitor :: Logger logger => Scheduler -> logger -> IO ()
runRtsMonitor sched logger = L.withLoggerLabel ("component", "rts-monitor") logger go
  where
    go l = getRTSStatsEnabled >>= \case
        False -> do
            logFunctionText l Warn "RTS Stats isn't enabled. Run with '+RTS -T' to enable it."
        True -> do
            scheduleEvery sched "Chainweb.Node.runRtsMonitor" 60_000_000 $ do
                logFunctionText l Debug "logging RTS stats"
                stats <- getRTSStats
                logFunctionJson logger Info stats

runQueueMonitor :: Logger logger => Scheduler -> logger -> CutDb logger -> IO ()
runQueueMonitor sched logger cutDb = L.withLoggerLabel ("component", "queue-monitor") logger go
  where
    go l = do
        scheduleEvery sched "ChainwebNode.runQueueMonitor" 60_000_000 $ do
            logFunctionText l Debug "logging cut queue stats"
            stats <- getQueueStats cutDb
            logFunctionJson logger Info stats

data DbStats = DbStats
    { dbStatsName :: !Text
    , dbStatsSize :: !Integer
    } deriving (Generic, NFData, ToJSON)

runDatabaseMonitor :: Logger logger => Scheduler -> logger -> FilePath -> FilePath -> IO ()
runDatabaseMonitor sched logger rocksDbDir pactDbDir = L.withLoggerLabel ("component", "database-monitor") logger go
  where
    go l = do
        scheduleEvery sched "ChainwebNode.runDatabaseMonitor" 1_200_000_000 {- 20 minutes -} $ do
            logFunctionText l Debug "logging database stats"
            logFunctionJson l Info . DbStats "rocksDb" =<< sizeOf rocksDbDir
            logFunctionJson l Info . DbStats "pactDb" =<< sizeOf pactDbDir
    sizeOf path = do
        dir <- doesDirectoryExist path
        file <- doesFileExist path
        if dir then
            fmap sum . traverse (sizeOf . (path </>)) =<< listDirectory path
        else if file then
            getFileSize path
        else
            pure 0

-- -------------------------------------------------------------------------- --
-- Run Node

node :: HasCallStack => HasVersion => Logger logger => ChainwebNodeConfiguration -> logger -> IO ()
node conf logger = do
    rocksDbDir <- getRocksDbDir conf
    pactDbDir <- getPactDbDir conf
    dbBackupsDir <- getBackupsDir conf
    withRocksDb' <-
        if _configReadOnlyReplay cwConf
        then
            withReadOnlyRocksDb <$ logFunctionText logger Info "Opening RocksDB in read-only mode"
        else
            return withRocksDb
    withRocksDb' rocksDbDir modernDefaultOptions $ \rocksDb -> do
        logFunctionText logger Info $ "opened rocksdb in directory " <> sshow rocksDbDir
        logFunctionText logger Debug $ "backup config: " <> sshow (_configBackup cwConf)
        runResourceT $ do
            sched <- withScheduler (logFunctionText logger) =<< liftIO backgroundJobsPoolSize
            liftIO $ withChainweb cwConf logger sched rocksDb pactDbDir dbBackupsDir $ \case
                RewoundToCut _ -> return ()
                StartedChainweb cw -> do
                    let telemetryEnabled =
                            _enableConfigEnabled $ _logConfigTelemetryBackend $ _nodeConfigLog conf
                        sched' = _chainwebScheduler cw
                    when telemetryEnabled $ do
                        runCutMonitor sched' (_chainwebLogger cw) (_cutResCutDb $ _chainwebCutResources cw)
                        runQueueMonitor sched' (_chainwebLogger cw) (_cutResCutDb $ _chainwebCutResources cw)
                        runRtsMonitor sched' (_chainwebLogger cw)
                        -- runBlockUpdateMonitor (_chainwebLogger cw) (_cutResCutDb $ _chainwebCutResources cw)
                        runDatabaseMonitor sched' (_chainwebLogger cw) rocksDbDir pactDbDir
                    -- we should probably push 'onReady' deeper here but this should be ok
                    runChainweb cw (\_ -> return ())
  where
    cwConf = _nodeConfigChainweb conf

    backgroundJobsPoolSize = case _configBackgroundJobsPoolSize cwConf of
        Just n -> return $! int n
        Nothing -> max 1 . (`div` 2) <$> getNumCapabilities

withNodeLogger
    :: HasVersion
    => LogConfig
    -> ChainwebConfiguration
    -> ChainwebVersion
    -> (L.Logger SomeLogMessage -> IO ())
    -> IO ()
withNodeLogger logCfg chainwebCfg v f = runManaged $ do

    -- This manager is used only for logging backends
    mgr <- liftIO HTTPS.newTlsManager

    -- Base Backend
    baseBackend <- managed
        $ withBaseHandleBackend "ChainwebApp" mgr pkgInfoScopes (_logConfigBackend logCfg)

    -- we don't log tx failures in replay
    let !txFailureHandler =
            if isJust (_cutInitialCutFile (_configCuts chainwebCfg))
                || isJust (_cutInitialBlockHeightLimit (_configCuts chainwebCfg))
                || _configReadOnlyReplay chainwebCfg
            then [dropLogHandler (Proxy :: Proxy PactTxFailureLog)]
            else []

    -- Telemetry Backends
    monitorBackend <- managed
        $ mkTelemetryLogger @CutHashes mgr teleLogConfig
    p2pInfoBackend <- managed
        $ mkTelemetryLogger @P2pSessionInfo mgr teleLogConfig
    rtsBackend <- managed
        $ mkTelemetryLogger @RTSStats mgr teleLogConfig
    counterBackend <- managed $ configureHandler
        (withJsonHandleBackend @CounterLog "counters" mgr pkgInfoScopes)
        teleLogConfig
    endpointBackend <- managed
        $ mkTelemetryLogger @PactCmdLog mgr teleLogConfig
    newBlockBackend <- managed
        $ mkTelemetryLogger @NewMinedBlock mgr teleLogConfig
    orphanedBlockBackend <- managed
        $ mkTelemetryLogger @OrphanedBlock mgr teleLogConfig
--     miningStatsBackend <- managed
--         $ mkTelemetryLogger @MiningStats mgr teleLogConfig
    requestLogBackend <- managed
        $ mkTelemetryLogger @RequestResponseLog mgr teleLogConfig
    queueStatsBackend <- managed
        $ mkTelemetryLogger @QueueStats mgr teleLogConfig
    traceBackend <- managed
        $ mkTelemetryLogger @Trace mgr teleLogConfig
    mempoolStatsBackend <- managed
        $ mkTelemetryLogger @MempoolStats mgr teleLogConfig
    blockUpdateBackend <- managed
        $ mkTelemetryLogger @BlockUpdate mgr teleLogConfig
    dbCacheBackend <- managed
        $ mkTelemetryLogger @DbCacheStats mgr teleLogConfig
    dbStatsBackend <- managed
        $ mkTelemetryLogger @DbStats mgr teleLogConfig
    p2pNodeStatsBackend <- managed
        $ mkTelemetryLogger @P2pNodeStats mgr teleLogConfig
    topLevelStatusBackend <- managed
        $ mkTelemetryLogger @ChainwebStatus mgr teleLogConfig
    pruneStatsBackend <- managed
        $ mkTelemetryLogger @PruneStats mgr teleLogConfig

    logger <- managed
        $ L.withLogger (_logConfigLogger logCfg) $ logHandles
            (concat
                [ [ logFilterHandle (_logConfigFilter logCfg) ]
                , txFailureHandler
                ,
                    [ logHandler monitorBackend
                    , logHandler p2pInfoBackend
                    , logHandler rtsBackend
                    , logHandler counterBackend
                    , logHandler endpointBackend
                    , logHandler newBlockBackend
                    , logHandler orphanedBlockBackend
                    -- , logHandler miningStatsBackend
                    , logHandler requestLogBackend
                    , logHandler queueStatsBackend
                    , logHandler traceBackend
                    , logHandler mempoolStatsBackend
                    , logHandler blockUpdateBackend
                    , logHandler dbCacheBackend
                    , logHandler dbStatsBackend
                    , logHandler p2pNodeStatsBackend
                    , logHandler topLevelStatusBackend
                    , logHandler pruneStatsBackend
                    ]
            ]) baseBackend

    liftIO $ f
        $ maybe id (\x -> addLabel ("cluster", toText x)) (_logConfigClusterId logCfg)
        $ addLabel ("chainwebVersion", sshow (_versionName v))
        $ logger
  where
    teleLogConfig = _logConfigTelemetryBackend logCfg

mkTelemetryLogger
    :: forall a b
    . (Typeable a, ToJSON a)
    => HTTP.Manager
    -> EnableConfig BackendConfig
    -> (Backend (JsonLog a) -> IO b)
    -> IO b
mkTelemetryLogger mgr = configureHandler
    $ withJsonHandleBackend @a (sshow $ typeRep $ Proxy @a) mgr pkgInfoScopes

-- -------------------------------------------------------------------------- --
-- Encode Package Info into Log mesage scopes

pkgInfoScopes :: [(Text, Text)]
pkgInfoScopes =
    [ ("revision", revision)
    , ("branch", branch)
    , ("compiler", compiler)
    , ("optimisation", optimisation)
    , ("architecture", arch)
    , ("package", package)
    ]

-- -------------------------------------------------------------------------- --
-- main

mainInfo :: ProgramInfo ChainwebNodeConfiguration
mainInfo = programInfoValidate
    "Chainweb Node"
    pChainwebNodeConfiguration
    defaultChainwebNodeConfiguration
    validateChainwebNodeConfiguration

handles :: [Handler a] -> IO a -> IO a
handles = flip catches

main :: IO ()
main = do
    installFatalSignalHandlers [ sigHUP, sigTERM, sigXCPU, sigXFSZ ]
    checkRLimits
    runWithPkgInfoConfiguration mainInfo pkgInfo $ \conf -> do
        withVersion (_configChainwebVersion $ _nodeConfigChainweb conf) $ do
            hSetBuffering stderr LineBuffering
            withNodeLogger (_nodeConfigLog conf) (_nodeConfigChainweb conf) implicitVersion $ \logger -> do
                logFunctionJson logger Info ProcessStarted
                handles
                    [ Handler $ \(e :: SomeAsyncException) ->
                        logFunctionJson logger Info (ProcessDied $ show e) >> throwIO e
                    , Handler $ \(e :: SomeException) ->
                        logFunctionJson logger Error (ProcessDied $ show e) >> throwIO e
                    ] $ do
                    void $ race (node conf logger) (gcRunner (logFunctionText logger))
    where
    gcRunner lf = runForever lf "GarbageCollect" $ do
        performMajorGC
        threadDelay (30 * 1_000_000)
