{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Chainweb.Chainweb.Configuration
-- Copyright: Copyright © 2021 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Chainweb.Chainweb.Configuration
(
-- * Payload Provider Config
PayloadProviderConfig
, GenericProviderConfig(..)
, _MinimalConfig
, _EvmConfig
, _PactConfig
, defaultPayloadProviderConfig
, defaultPayloadProviderConfigWithMinimal
, defaultPayloadProviderConfigWithPact
, validatePayloadProviderConfig

-- * Throttling Configuration
, ThrottlingConfig(..)
, throttlingRate
, throttlingPeerRate
, throttlingMempoolRate
, defaultThrottlingConfig

-- * Cut Configuration
, CutConfig(..)
, cutFetchTimeout
, cutInitialBlockHeightLimit
, defaultCutConfig
, pCutConfig

-- * Service API Configuration
, ServiceApiConfig(..)
, serviceApiConfigPort
, serviceApiConfigInterface
, serviceApiConfigHeaderStream
, defaultServiceApiConfig
, pServiceApiConfig

-- * Backup configuration
, BackupApiConfig(..)
, configBackupApi
, BackupConfig(..)
, defaultBackupConfig

-- * Pruning configuration
, PruneConfig(..)
, defaultPruneConfig

-- * Chainweb Configuration
, ChainwebConfiguration(..)
, configChainwebVersion
, configCuts
, configMining
, configP2p
, configThrottling
, configReorgLimit
, configBackup
, configServiceApi
, configReadOnlyReplay
, configSyncChains
, configPayloadProviders
, configBackgroundJobsPoolSize
, defaultChainwebConfiguration
, pChainwebConfiguration
, validateChainwebConfiguration

) where

import Chainweb.BlockHeaderDB.PruneForks qualified as PruneForks
import Chainweb.BlockHeight
import Chainweb.Difficulty
import Chainweb.HostAddress
import Chainweb.Miner.Config
import Chainweb.Pact.Types (RewindLimit)
import Chainweb.Pact.Types (defaultReorgLimit)
import Chainweb.Pact.Payload.RestAPI (PayloadBatchLimit(..), defaultServicePayloadBatchLimit)
import Chainweb.PayloadProvider.EVM (EvmProviderConfig, defaultEvmProviderConfig, validateEvmProviderConfig)
import Chainweb.PayloadProvider.Minimal (MinimalProviderConfig, defaultMinimalProviderConfig, validateMinimalProviderConfig)
import Chainweb.PayloadProvider.Pact.Configuration
import Chainweb.Utils
import Chainweb.Version
import Chainweb.Version.Development
import Chainweb.Version.EvmDevelopment
import Chainweb.Version.EvmDevelopmentSingleton
import Chainweb.Version.Mainnet
import Chainweb.Version.RecapDevelopment
import Chainweb.Version.Registry (findKnownVersion, knownVersions, knownHighestChainId, knownLowestChainId)
import Configuration.Utils hiding (Error, Lens', disabled)
import Control.Lens hiding ((.=), (<.>))
import Control.Monad
import Control.Monad.Except
import Control.Monad.Writer
import Data.Aeson.Types qualified as A
import Data.Aeson.KeyMap qualified as KM
import Data.Foldable
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.Maybe
import Data.Text qualified as T
import GHC.Generics hiding (from, to)
import GHC.Natural (Natural)
import Network.Wai.Handler.Warp hiding (Port)
import P2P.Node.Configuration
import Prelude hiding (log)
import System.Directory

-- -------------------------------------------------------------------------- --
-- Payload Provider Configuration

-- | Payload Provider Configurations
--
-- When a payload provider is enabled for a chain it must match the payload
-- provider type for the respective chain.
--
data GenericProviderConfig = MinimalConfig MinimalProviderConfig
                           | PactConfig PactProviderConfig
                           | EvmConfig EvmProviderConfig
                        deriving (Show, Eq, Generic)

makePrisms ''GenericProviderConfig

type PayloadProviderConfig = ChainRangesMap GenericProviderConfig

-- A set of default Providers
-- Should the default be Minimal on all chains or empty ?
defaultPayloadProviderConfig :: PayloadProviderConfig
-- defaultPayloadProviderConfig = rangesMapAllChains (MinimalConfig defaultMinimalProviderConfig)
defaultPayloadProviderConfig = mempty

-- Minimal providers on all chains
defaultPayloadProviderConfigWithMinimal ::PayloadProviderConfig
defaultPayloadProviderConfigWithMinimal = rangesMapAllChains $ MinimalConfig defaultMinimalProviderConfig

-- Pact providers on all chains
defaultPayloadProviderConfigWithPact ::PayloadProviderConfig
defaultPayloadProviderConfigWithPact = rangesMapAllChains $ PactConfig defaultPactProviderConfig


minimalTag :: T.Text
minimalTag = "minimal"

pactTag :: T.Text
pactTag = "pact"

evmTag :: T.Text
evmTag = "evm"

instance ToJSON GenericProviderConfig where
    toJSON (MinimalConfig c) = tag minimalTag c
    toJSON (PactConfig c)    = tag pactTag c
    toJSON (EvmConfig c)     = tag evmTag c

tag :: ToJSON v => T.Text -> v -> Value
tag t v = case toJSON v of
    Object l -> Object $ KM.insert "type" (toJSON t) l
    x -> x

instance FromJSON GenericProviderConfig where
    parseJSON = withObject "ProviderConfig" $ \o ->
        (o .: "type" :: A.Parser T.Text) >>= \case
            t | t == minimalTag -> MinimalConfig <$> (parseJSON (Object o) <*> pure defaultMinimalProviderConfig)
              | t == pactTag ->    PactConfig    <$> (parseJSON (Object o) <*> pure defaultPactProviderConfig)
              | t == evmTag ->     EvmConfig     <$> (parseJSON (Object o) <*> pure defaultEvmProviderConfig)
              | otherwise -> fail "Unknown provider type"

pPayloadProviderConfig :: MParser PayloadProviderConfig
pPayloadProviderConfig = ((<>) . fromMaybe mempty)
                         <$> (optional $ jsonOption ( long "providers-config" <> help "JSON config of providers"))


pPayloadProviderDisable :: MParser PayloadProviderConfig
pPayloadProviderDisable = foldl (.) id <$> traverse (\c -> doDisable c <$> disableSwitch c) cids
  where
    cids = [knownLowestChainId .. knownHighestChainId]
    doDisable cid yesno = if yesno then rangesMapRemove cid else id
    disableSwitch cid = switch
        % long ("disable-chain-" <> T.unpack (toText cid))
        <> help ("disabled the payload provider for this chain")
        <> mconcat [ hidden <> internal | chainIdInt @Int cid /= 0 ]

validatePayloadProviderConfig :: HasVersion => ConfigValidation PayloadProviderConfig []
validatePayloadProviderConfig conf = do
    unless (rangesMapNoOverLap conf) $
        tell [ "Overlapping Payload providers ranges: not fatal. Check config again though"]

    void $ itraverse validateSpecificProviderConfig $ toChainMap lowestChainId highestChainId conf
    where
        validateSpecificProviderConfig cid _conf =
            case (_conf, payloadProviderTypeForChain cid) of
                -- Config matches with what is defined in the version => Validate correspondings configs
                (MinimalConfig c, MinimalProvider) -> validateMinimalProviderConfig c
                (PactConfig c, PactProvider) -> validatePactProviderConfig cid c
                (EvmConfig c, EvmProvider _) -> validateEvmProviderConfig cid c
                -- Mismatch
                (_, e) -> do
                    tell [ configTypeName _conf <> "provider configured for chain " <> sshow cid <> ": " <> sshow _conf ]
                    -- Should we throw an error error here ? Or let the node manage it later by disabling chains
                    throwError $ mconcat $
                        [ "Wrong payload provider type configuration for chain " <> sshow cid
                        , ". Expected " <> sshow e <> " but found " <> configTypeName _conf
                        ]
        configTypeName = \case
            MinimalConfig _ -> "Minimal"
            PactConfig _ -> "Pact"
            EvmConfig _ -> "EVM"

-- -------------------------------------------------------------------------- --
-- Throttling Configuration

data ThrottlingConfig = ThrottlingConfig
    { _throttlingRate :: !Double
    , _throttlingPeerRate :: !Double
        -- ^ This should throttle aggressively. This endpoint does an expensive
        -- check of the client. And we want to keep bad actors out of the
        -- system. There should be no need for a client to call this endpoint on
        -- the same node more often than at most few times peer minute.
    , _throttlingMempoolRate :: !Double
    }
    deriving stock (Eq, Show)

makeLenses ''ThrottlingConfig

defaultThrottlingConfig :: ThrottlingConfig
defaultThrottlingConfig = ThrottlingConfig
    { _throttlingRate = 50 -- per second, in a 100 burst
    , _throttlingPeerRate = 11 -- per second, 1 for each p2p network
    , _throttlingMempoolRate = 20 -- one every seconds per mempool.
    }

instance ToJSON ThrottlingConfig where
    toJSON o = object
        [ "global" .= _throttlingRate o
        , "putPeer" .= _throttlingPeerRate o
        , "mempool" .= _throttlingMempoolRate o
        ]

instance FromJSON (ThrottlingConfig -> ThrottlingConfig) where
    parseJSON = withObject "ThrottlingConfig" $ \o -> id
        <$< throttlingRate ..: "global" % o
        <*< throttlingPeerRate ..: "putPeer" % o
        <*< throttlingMempoolRate ..: "mempool" % o

-- -------------------------------------------------------------------------- --
-- Cut Configuration

data CutConfig = CutConfig
    { _cutFetchTimeout :: !Int
    , _cutInitialBlockHeightLimit :: !(Maybe BlockHeight)
    , _cutInitialCutFile :: !(Maybe FilePath)
    } deriving (Eq, Show)

makeLenses ''CutConfig

instance ToJSON CutConfig where
    toJSON o = object
        [ "fetchTimeout" .= _cutFetchTimeout o
        , "initialBlockHeightLimit" .= _cutInitialBlockHeightLimit o
        , "initialCutFile" .= _cutInitialCutFile o
        ]

instance FromJSON (CutConfig -> CutConfig) where
    parseJSON = withObject "CutConfig" $ \o -> id
        <$< cutFetchTimeout ..: "fetchTimeout" % o
        <*< cutInitialBlockHeightLimit ..: "initialBlockHeightLimit" % o
        <*< cutInitialCutFile ..: "initialCutFile" % o

defaultCutConfig :: CutConfig
defaultCutConfig = CutConfig
    { _cutFetchTimeout = 4_125_000
    , _cutInitialBlockHeightLimit = Nothing
    , _cutInitialCutFile = Nothing
    }

pCutConfig :: MParser CutConfig
pCutConfig = id
    <$< cutFetchTimeout .:: option auto
        % long "cut-fetch-timeout"
        <> help "The timeout for processing new cuts in microseconds"
    <*< cutInitialBlockHeightLimit .:: fmap (Just . BlockHeight) . option auto
        % long "initial-block-height-limit"
        <> help "Reset initial cut to this block height."
        <> metavar "INT"
    <*< cutInitialCutFile .:: fmap Just . textOption
        % long "initial-cut-file"
        <> help "When --initial-cut-file is given, use the cut in the given file as the initial cut. Note that this will not contact the P2P network."

-- -------------------------------------------------------------------------- --
-- Service API Configuration

data ServiceApiConfig = ServiceApiConfig
    { _serviceApiConfigPort :: !Port
        -- ^ The public host address for service APIs.
        -- A port number of 0 means that a free port is assigned by the system.
        --
        -- The default is 1917

    , _serviceApiConfigInterface :: !HostPreference
        -- ^ The network interface that the service APIs are bound to. Default is to
        -- bind to all available interfaces ('*').
    , _serviceApiConfigValidateSpec :: !Bool
        -- ^ Validate requests and responses against the latest OpenAPI specification.
        -- Disabled by default for performance reasons

    , _serviceApiPayloadBatchLimit :: PayloadBatchLimit
        -- ^ maximum size for payload batches on the service API. Default is
        -- 'Chainweb.Pact.Payload.RestAPI.defaultServicePayloadBatchLimit'.
    , _serviceApiConfigHeaderStream :: !Bool
        -- ^ whether to serve a header update stream endpoint.
    }
    deriving (Show, Eq, Generic)

makeLenses ''ServiceApiConfig

defaultServiceApiConfig :: ServiceApiConfig
defaultServiceApiConfig = ServiceApiConfig
    { _serviceApiConfigPort = 1848
    , _serviceApiConfigInterface = "*"
    , _serviceApiConfigValidateSpec = False
    , _serviceApiPayloadBatchLimit = defaultServicePayloadBatchLimit
    , _serviceApiConfigHeaderStream = False
    }

instance ToJSON ServiceApiConfig where
    toJSON o = object
        [ "port" .= _serviceApiConfigPort o
        , "interface" .= hostPreferenceToText (_serviceApiConfigInterface o)
        , "validateSpec" .= _serviceApiConfigValidateSpec o
        , "payloadBatchLimit" .= _serviceApiPayloadBatchLimit o
        , "headerStream" .= _serviceApiConfigHeaderStream o
        ]

instance FromJSON (ServiceApiConfig -> ServiceApiConfig) where
    parseJSON = withObject "ServiceApiConfig" $ \o -> id
        <$< serviceApiConfigPort ..: "port" % o
        <*< setProperty serviceApiConfigInterface "interface" (parseJsonFromText "interface") o
        <*< serviceApiConfigValidateSpec ..: "validateSpec" % o
        <*< serviceApiPayloadBatchLimit ..: "payloadBatchLimit" % o
        <*< serviceApiConfigHeaderStream ..: "headerStream" % o

pServiceApiConfig :: MParser ServiceApiConfig
pServiceApiConfig = id
    <$< serviceApiConfigPort .:: pPort service
    <*< serviceApiConfigInterface .:: textOption
        % prefixLong service "interface"
        <> suffixHelp service "interface that the service Rest API binds to (see HostPreference documentation for details)"
    -- serviceApiBackups isn't supported on the command line
    <*< serviceApiPayloadBatchLimit .:: fmap PayloadBatchLimit . option auto
        % prefixLong service "payload-batch-limit"
        <> suffixHelp service "upper limit for the size of payload batches on the service API"
    <*< serviceApiConfigValidateSpec .:: enableDisableFlag
        % prefixLong service "validate-spec"
        <> internal -- hidden option, for expert use
    <*< serviceApiConfigHeaderStream .:: boolOption_
        % prefixLong service "header-stream"
        <> help "whether to enable an endpoint for streaming block updates"

  where
    service = Just "service"

-- -------------------------------------------------------------------------- --
-- Backup configuration

data BackupApiConfig = BackupApiConfig
    deriving (Show, Eq, Generic)

defaultBackupApiConfig :: BackupApiConfig
defaultBackupApiConfig = BackupApiConfig

data BackupConfig = BackupConfig
    { _configBackupApi :: !(EnableConfig BackupApiConfig)
    , _configBackupDirectory :: !(Maybe FilePath)
    -- ^ Should be a path in the same partition as the database directory to
    --   avoid the slow path of the rocksdb checkpoint mechanism.
    }
    deriving (Show, Eq, Generic)

defaultBackupConfig :: BackupConfig
defaultBackupConfig = BackupConfig
    { _configBackupApi = EnableConfig False defaultBackupApiConfig
    , _configBackupDirectory = Nothing
    }

makeLenses ''BackupApiConfig
makeLenses ''BackupConfig

instance ToJSON BackupApiConfig where
    toJSON _cfg = toJSON $ object [ ]

instance FromJSON (BackupApiConfig -> BackupApiConfig) where
    parseJSON = withObject "BackupApiConfig" $ \_ -> return id

pBackupApiConfig :: MParser BackupApiConfig
pBackupApiConfig = pure id

instance ToJSON BackupConfig where
    toJSON cfg = object
        [ "api" .= _configBackupApi cfg
        , "directory" .= _configBackupDirectory cfg
        ]

instance FromJSON (BackupConfig -> BackupConfig) where
    parseJSON = withObject "BackupConfig" $ \o -> id
        <$< configBackupApi %.: "api" % o
        <*< configBackupDirectory ..: "directory" % o

pBackupConfig :: MParser BackupConfig
pBackupConfig = id
    <$< configBackupApi %:: pEnableConfig "backup-api" pBackupApiConfig
    <*< configBackupDirectory .:: fmap Just % textOption
        % prefixLong backup "directory"
        <> suffixHelp backup "Directory in which backups will be placed when using the backup API endpoint"
  where
    backup = Just "backup"

-- -------------------------------------------------------------------------- --
-- Pruning Configuration

data PruneConfig = PruneConfig
    { _configPruneCommand :: PruneForks.DoPrune
    }
    deriving (Show, Eq)

defaultPruneConfig :: PruneConfig
defaultPruneConfig = PruneConfig
    { _configPruneCommand = PruneForks.Prune
    }
makeLenses ''PruneConfig

instance ToJSON PruneConfig where
    toJSON cfg = object [ "command" .= _configPruneCommand cfg ]
instance FromJSON PruneConfig where
    parseJSON = withObject "PruneConfig" $ \o ->
        PruneConfig <$> o .: "command"
instance FromJSON (PruneConfig -> PruneConfig) where
    parseJSON = withObject "PruneConfig" $ \o -> id
        <$< configPruneCommand ..: "command" % o

pPruneConfig :: MParser PruneConfig
pPruneConfig = id
    <$< parserOptionGroup "Pruning"
        (configPruneCommand .:: textOption (long "pruning-command"))

-- -------------------------------------------------------------------------- --
-- Chainweb Configuration

data ChainwebConfiguration = ChainwebConfiguration
    { _configChainwebVersion :: !ChainwebVersion
    , _configCuts :: !CutConfig
    , _configMining :: !MiningConfig
    , _configP2p :: !P2pConfiguration
    , _configThrottling :: !ThrottlingConfig
    , _configReorgLimit :: !RewindLimit
    , _configBackup :: !BackupConfig
    , _configServiceApi :: !ServiceApiConfig
    , _configPayloadProviders :: PayloadProviderConfig
    , _configPrune :: !PruneConfig

    -- The following properties are deprecated: history replay should not be
    -- part of normal operation mode. It should probably use a completely
    -- separate configuration.

    , _configReadOnlyReplay :: !Bool
        -- ^ do a read-only replay using the cut db params for the block heights
    , _configSyncChains :: !(Maybe [ChainId])
        -- ^ the only chains to be synchronized on startup to the latest cut.
        --   if unset, all chains will be synchronized.

    , _configBackgroundJobsPoolSize :: !(Maybe Natural)
        -- ^ the number of worker threads of the scheduler for background
        --   jobs. If unset, a default of 'max 1 (getNumCapabilities div 2)'
        --   is used.

    } deriving (Show, Eq, Generic)

makeLenses ''ChainwebConfiguration

validateChainwebConfiguration :: ConfigValidation ChainwebConfiguration []
validateChainwebConfiguration c = do
    validateChainwebVersion (_configChainwebVersion c)
    withVersion (_configChainwebVersion c) $ do
        validateMinerConfig (_configMining c)
        validateBackupConfig (_configBackup c)
        unless (c ^. configChainwebVersion . versionDefaults . disablePeerValidation) $
            validateP2pConfiguration (_configP2p c)
        validatePayloadProviderConfig (_configPayloadProviders c)
        mapM_ checkBackgroundJobsPoolSize (_configBackgroundJobsPoolSize c)
  where
    checkBackgroundJobsPoolSize n =
        unless (n >= 1) $
            throwError "background jobs pool size must be at least 1"

validateChainwebVersion :: ConfigValidation ChainwebVersion []
validateChainwebVersion v = do
    unless (isDevelopment || elem v knownVersions) $
        throwError $ T.unwords
            [ "Specifying version properties is only legal with chainweb-version"
            , "set to recap-development or development, but version is set to"
            , sshow (_versionName v)
            ]
    where
    isDevelopment = _versionCode v `elem`
        [_versionCode dv | dv <-
            [recapDevnet, devnet, evmDevnet, evmDevnetSingleton, evmDevnetPair]
        ]

validateBackupConfig :: ConfigValidation BackupConfig []
validateBackupConfig c =
    for_ (_configBackupDirectory c) $ \dir -> do
        liftIO $ createDirectoryIfMissing True dir
        perms <- liftIO (getPermissions dir)
        unless (writable perms) $
            throwError $ "Backup directory " <> T.pack dir <> " is not writable"

defaultChainwebConfiguration :: ChainwebVersion -> ChainwebConfiguration
defaultChainwebConfiguration v = ChainwebConfiguration
    { _configChainwebVersion = v
    , _configCuts = defaultCutConfig
    , _configMining = defaultMining
    , _configP2p = defaultP2pConfiguration
    , _configThrottling = defaultThrottlingConfig
    , _configReorgLimit = defaultReorgLimit
    , _configServiceApi = defaultServiceApiConfig
    , _configReadOnlyReplay = False
    , _configPrune = defaultPruneConfig
    , _configSyncChains = Nothing
    , _configBackup = defaultBackupConfig
    -- TODO => Perhaps it should be adjusted depending on Version
    , _configPayloadProviders = defaultPayloadProviderConfig
    , _configBackgroundJobsPoolSize = Nothing
    }

instance ToJSON ChainwebConfiguration where
    toJSON o = object
        [ "chainwebVersion" .= _versionName (_configChainwebVersion o)
        , "cuts" .= _configCuts o
        , "mining" .= _configMining o
        , "p2p" .= _configP2p o
        , "throttling" .= _configThrottling o
        , "reorgLimit" .= _configReorgLimit o
        , "serviceApi" .= _configServiceApi o
        , "readOnlyReplay" .= _configReadOnlyReplay o
        , "syncChains" .= _configSyncChains o
        , "backup" .= _configBackup o
        , "payloadProviders" .= _configPayloadProviders o
        , "pruning" .= _configPrune o
        , "backgroundJobsPoolSize" .= _configBackgroundJobsPoolSize o
        ]

instance FromJSON ChainwebConfiguration where
    parseJSON = fmap ($ defaultChainwebConfiguration Mainnet01) . parseJSON

instance FromJSON (ChainwebConfiguration -> ChainwebConfiguration) where
    parseJSON = withObject "ChainwebConfiguration" $ \o -> id
        <$< setProperty configChainwebVersion "chainwebVersion"
            (findKnownVersion <=< parseJSON) o
        <*< configCuts %.: "cuts" % o
        <*< configMining %.: "mining" % o
        <*< configP2p %.: "p2p" % o
        <*< configThrottling %.: "throttling" % o
        <*< configReorgLimit ..: "reorgLimit" % o
        <*< configServiceApi %.: "serviceApi" % o
        <*< configReadOnlyReplay ..: "readOnlyReplay" % o
        <*< configSyncChains ..: "syncChains" % o
        <*< configBackup %.: "backup" % o
        <*< configPayloadProviders %.: "payloadProviders" % o
        <*< configPrune %.: "pruning" % o
        <*< configBackgroundJobsPoolSize ..: "backgroundJobsPoolSize" % o

pChainwebConfiguration :: MParser ChainwebConfiguration
pChainwebConfiguration = id
    <$< configChainwebVersion %:: parseVersion
    <*< parserOptionGroup "P2P" (configP2p %:: pP2pConfiguration)
    <*< configReorgLimit .:: jsonOption
        % long "reorg-limit"
        <> help "Max allowed reorg depth.\
                \ Consult https://github.com/kadena-io/chainweb-node/blob/master/docs/RecoveringFromDeepForks.md for\
                \ more information. "
    <*< parserOptionGroup "Cut Processing" (configCuts %:: pCutConfig)
    <*< parserOptionGroup "Service API" (configServiceApi %:: pServiceApiConfig)
    <*< parserOptionGroup "Mining Coordination" (configMining %:: pMiningConfig)
    <*< configReadOnlyReplay .:: boolOption_
        % long "read-only-replay"
        <> help "Replay the block history non-destructively"
    <*< configSyncChains .:: fmap Just % jsonOption
        % long "sync-chains"
        <> help "The only Pact databases to synchronize. If empty or unset, all chains will be synchronized."
        <> metavar "JSON list of chain ids"
    <*< parserOptionGroup "Backup" (configBackup %:: pBackupConfig)

    -- FIXME support payload providers
    <*< configPayloadProviders %:: (pPayloadProviderDisable <*< pPayloadProviderConfig)
    <*< configPrune %:: pPruneConfig
    <*< configBackgroundJobsPoolSize .:: fmap Just % option auto
        % prefixLong (Just "background-jobs") "pool-size"
        <> suffixHelp (Just "background-jobs") "size of the thread pool for the background jobs scheduler"

parseVersion :: MParser ChainwebVersion
parseVersion = constructVersion
    <$> optional
        (option (findKnownVersion =<< textReader)
            % long "chainweb-version"
            <> short 'v'
            <> help "the chainweb version that this node is using"
            <> metavar (T.unpack $
                "[" <> T.intercalate "," (getChainwebVersionName . _versionName <$> knownVersions) <> "]")
        )
    <*> optional (textOption @Fork (long "fork-upper-bound" <> help "(development mode only) the latest fork the node will enable"))
    <*> optional (BlockDelay <$> textOption (long "block-delay" <> help "(development mode only) the block delay in seconds per block"))
    <*> switch (long "disable-pow" <> help "(development mode only) disable proof of work check")
    where
    constructVersion cliVersion fub bd disablePow' oldVersion = winningVersion
        & versionBlockDelay .~ fromMaybe (_versionBlockDelay winningVersion) bd
        & versionForks %~ HM.filterWithKey (\fork _ -> fork <= fromMaybe maxBound fub)
        & versionUpgrades .~
            maybe (_versionUpgrades winningVersion) (\fub' ->
                ChainMap $ HM.mapWithKey
                    (\cid _ ->
                        let currentUpgrades =  winningVersion ^?! versionUpgrades . atChain cid
                        in case winningVersion ^?! versionForks . at fub' . _Just . atChain cid of
                            ForkNever -> error "Chainweb.Chainweb.Configuration.parseVersion: the fork upper bound never occurs in this version."
                            ForkAtBlockHeight fubHeight -> HM.filterWithKey (\bh _ -> bh <= fubHeight) currentUpgrades
                            ForkAtGenesis -> winningVersion ^?! versionUpgrades . atChain cid
                            ForkAtForkNumber _ -> currentUpgrades -- For now, version upgrades were only allowed at block heights
                    )
                    (HS.toMap (withVersion winningVersion chainIds))
            ) fub
        & versionCheats . disablePow .~ disablePow'
        where
        winningVersion = fromMaybe oldVersion cliVersion
