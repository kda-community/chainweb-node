{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Chainweb.Test.MultiNode
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- # Test Configuration
--
-- The test runs
--
-- * for a short time,
-- * on a small chain,
-- * over a fast and reliable network,
-- * with a limited number of nodes.
--
-- The configuration defines a scaled down, accelerated chain that tries to
-- similulate a full-scale chain in a miniaturized settings.
--
module Chainweb.Test.MultiNode
  ( test
  , replayTest
-- TODO: PP
--   , compactAndResumeTest
  , pactImportTest
-- TODO: PP
--   , compactLiveNodeTest
  ) where

import Control.Concurrent
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeader.Genesis.InstantTimedCPM0Payload qualified as IN0
import Chainweb.BlockHeader.Genesis.InstantTimedCPM1to9Payload qualified as INN
import Chainweb.BlockHeight
import Chainweb.ChainId
import Chainweb.Chainweb
import Chainweb.Chainweb.Configuration
import Chainweb.Chainweb.CutResources
import Chainweb.Chainweb.PeerResources
import Chainweb.Cut
import Chainweb.Cut.CutHashes
import Chainweb.CutDB
import Chainweb.Graph
import Chainweb.Logger
import Chainweb.Miner.Config
import Chainweb.Miner.Pact
import Chainweb.Pact.Backend.PactState (allChains, getLatestBlockHeight, getLatestPactStateAtDiffable, TableDiffable(..), addChainIdLabel)
import Chainweb.Pact.Backend.PactState.EmbeddedSnapshot (Snapshot(..))
import Chainweb.Pact.Backend.PactState.GrandHash.Algorithm (ChainGrandHash(..))
import Chainweb.Pact.Backend.PactState.GrandHash.Calc qualified as GrandHash.Calc
import Chainweb.Pact.Backend.PactState.GrandHash.Import qualified as GrandHash.Import
import Chainweb.Pact.Backend.PactState.GrandHash.Utils qualified as GrandHash.Utils
import Chainweb.PayloadProvider.Pact.Configuration (defaultPactProviderConfig, PactProviderConfig (_pactConfigGenesisPayload), _pactConfigMiner)
import Chainweb.Scheduler (Scheduler, withScheduler)
import Chainweb.Storage.Table (IterableTable(withTableIterator), Casify)
import Chainweb.Storage.Table.RocksDB
import Chainweb.Test.P2P.Peer.BootstrapConfig
import Chainweb.Test.Utils
import Chainweb.Time (Seconds(..), Time(..), TimeSpan(..), getCurrentTimeIntegral)
import Chainweb.Utils
import Chainweb.Version
import Chainweb.Version.Utils
import Chainweb.WebBlockHeaderDB
import Control.Concurrent.Async
import Control.DeepSeq
import Control.Exception
import Control.Lens (set, view, (^?!), ix)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Aeson (ToJSON, Value, object, (.=))
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as Base16
import Data.Foldable
import Data.Function
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.Hashable
import Data.IORef
import Data.List qualified as L
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T
import Database.SQLite3.Direct (Database)
import GHC.Generics
import Numeric.Natural
import P2P.Node.Configuration
import P2P.Peer
import Pact.JSON.Encode qualified as J
import Patience.Map qualified as Patience
import Prelude hiding (log)
import Streaming.Prelude qualified as S
import System.FilePath
import System.IO.Temp
import System.LogLevel
import System.Timeout
import Test.Tasty.HUnit

-- | Integer node identifier; node 0 is the bootstrap.
newtype NodeId = NodeId Int
    deriving newtype (Eq, Hashable, NFData, Show)
bootstrapNodeId :: NodeId
bootstrapNodeId = NodeId 0
instance HasTextRepresentation NodeId where
    fromText = fmap NodeId . fromText
    toText (NodeId i) = toText i

-- -------------------------------------------------------------------------- --
-- * Configuration
--
-- This test runs
--
-- * for a short time,
-- * on a small chain,
-- * over a fast and reliable network,
-- * with a limited number of nodes.
--
-- The configuration defines a scaled down, accelerated chain that tries to
-- similulate a full-scale chain in a miniaturized settings.
--

-- | Test Configuration for a scaled down Test chainweb.
--
multiConfig
    :: HasVersion
    => Natural
        -- ^ number of node
    -> ChainwebConfiguration
multiConfig n = defaultChainwebConfiguration implicitVersion
    & set (configP2p . p2pConfigPeer . peerConfigHost) host
    & set (configP2p . p2pConfigPeer . peerConfigPort) 0
    & set (configP2p . p2pConfigPeer . peerConfigInterface) interface
        -- Only listen on the loopback device. On Mac OS X this prevents the
        -- firewall dialog form poping up.

    . set (configP2p . p2pConfigIgnoreBootstrapNodes) True
        -- The bootstrap peer info is set later after the bootstrap nodes
        -- has started and got its port assigned.

    . set (configP2p . p2pConfigMaxPeerCount) (int $ n * 2)
        -- We make room for all test peers in peer db.

    . set (configP2p . p2pConfigMaxSessionCount) 4
        -- We set this to a low number in order to keep the network sparse (or
        -- at last no being a clique) and to also limit the number of
        -- port allocations

    . set (configP2p . p2pConfigSessionTimeout) 20
        -- Use short sessions to cover session timeouts and setup logic in the
        -- test.

    . set (configP2p . p2pConfigBootstrapReachability) 0
        -- disable reachability test, which is unreliable during testing

    . set (configMining . miningCoordination . coordinationEnabled) True
    . set (configMining . miningInNode) miner

    & set configThrottling throttling
        -- throttling is effectively disabled to not slow down the test nodes

    & set (configServiceApi . serviceApiConfigPort) 0
    & set (configServiceApi . serviceApiConfigInterface) interface
    & set (configCuts . cutFetchTimeout) 10_000_000
    & set (configPayloadProviders . payloadProviderConfigPact)
        (onChains $
            [(cid, defaultPactProviderConfig
                { _pactConfigGenesisPayload = Just gp
                , _pactConfigMiner = Just noMiner
                })
            | (cid, gp) <-
                (unsafeChainId 0, IN0.payloadBlock)
                : [(unsafeChainId cid, INN.payloadBlock) | cid <- [1..19]]
            ])
  where
    miner = NodeMiningConfig
        { _nodeMiningEnabled = True
        , _nodeTestMiners = MinerCount n
        }

    throttling = defaultThrottlingConfig
        { _throttlingRate = 10_000 -- per second
        , _throttlingPeerRate = 10_000 -- per second, one for each p2p network
        }

multiConfigBuilders :: HasVersion => Natural -> [ChainwebConfiguration -> ChainwebConfiguration]
multiConfigBuilders n = replicate (int n) $ const (multiConfig n)

-- | Configure a bootstrap node
--
multiBootstrapConfig
    :: ChainwebConfiguration
    -> ChainwebConfiguration
multiBootstrapConfig conf = conf
    & set (configP2p . p2pConfigPeer) peerConfig
    & set (configP2p . p2pConfigKnownPeers) []
  where
    peerConfig = head (testBootstrapPeerConfig $ _configChainwebVersion conf)
        & set peerConfigPort 0
        -- Normally, the port of bootstrap nodes is hard-coded. But in
        -- test-suites that may run concurrently we want to use a port that is
        -- assigned by the OS.

        & set peerConfigHost host
        & set peerConfigInterface interface

-- -------------------------------------------------------------------------- --
-- Minimal Node Setup that logs conensus state to the given mvar

harvestConsensusState
    :: HasVersion
    => GenericLogger
    -> MVar ConsensusState
    -> NodeId
    -> StartedChainweb logger
    -> RocksDb
    -> IO ()
harvestConsensusState _ _ _ (RewoundToCut _) _ =
    error "harvestConsensusState: doesn't work when replaying, replays don't do consensus"
harvestConsensusState logger stateVar nid (StartedChainweb cw) rdb = do
    sampler <- forkIO $ do
        let cutdb = view (chainwebCutResources . cutResCutDb) cw
        forever $ do
            threadDelay 100_000
            c <- _cut cutdb
            Time (TimeSpan us) <- getCurrentTimeIntegral
            let t = int us / 1_000_000 :: Double
            modifyMVar_ stateVar $ \s -> return $! s
                { _stateCutTrace = HM.insertWith (\new old -> old <> new) nid [(t, _cutHeight c)] (_stateCutTrace s)
                }
    runChainweb cw (\_ -> return ()) `finally` do
        killThread sampler
        logFunctionText logger Info "write sample data"
        modifyMVar_ stateVar $
            sampleConsensusState
                nid
                (view (chainwebCutResources . cutResCutDb . cutDbWebBlockHeaderDb) cw)
                (view (chainwebCutResources . cutResCutDb) cw)
                (cutHashesTable rdb)
        logFunctionText logger Info "assert on cut count"
        logFunctionText logger Info "shutdown node"

multiNode
    :: LogLevel
    -> MVar PeerInfo
    -> ChainwebConfiguration
    -> Scheduler
    -> RocksDb
    -> FilePath
    -> NodeId
        -- ^ Unique node id. Node id 0 is used for the bootstrap node
    -> (forall logger. NodeId -> StartedChainweb logger -> RocksDb -> IO ())
    -> IO ()
multiNode loglevel bootstrapPeerInfoVar conf sched rdb pactDbDir nid inner = do
    withSystemTempDirectory "multiNode-backup-dir" $ \backupTmpDir ->
        withChainweb conf logger sched namespacedNodeRocksDb (pactDbDir </> show nid) backupTmpDir $ \cw -> do
            case cw of
                StartedChainweb cw' ->
                    when (nid == bootstrapNodeId) $ putMVar bootstrapPeerInfoVar
                        $ view (chainwebPeer . peerResPeer . peerInfo) cw'
                RewoundToCut _ -> return ()
            inner nid cw namespacedNodeRocksDb
  where
    logger :: GenericLogger
    logger = addLabel ("node", toText nid) $ genericLogger loglevel T.putStrLn

    namespacedNodeRocksDb = rdb { _rocksDbNamespace = T.encodeUtf8 $ toText nid }

-- -------------------------------------------------------------------------- --
-- Run Nodes

runNodes
    :: LogLevel
    -> (T.Text -> IO ())
    -> ChainwebVersion
    -> [ChainwebConfiguration -> ChainwebConfiguration]
        -- ^ number of nodes
    -> RocksDb
    -> FilePath
    -> (forall logger. NodeId -> StartedChainweb logger -> RocksDb -> IO ())
    -> IO ()
runNodes loglevel write v confBuilders rdb pactDbDir inner = runResourceT $ do
    -- NOTE: pact is enabled until we have a good way to disable it globally in
    -- "Chainweb.Chainweb".
    --
    -- TODO: disable pact for these tests
    --

    sched <- withScheduler (logFunctionText $ genericLogger loglevel write) 2
    bootstrapPortVar <- liftIO newEmptyMVar
        -- this is a hack for testing: normally bootstrap node peer infos are
        -- hardcoded. To avoid conflicts in concurrent test runs we extract an
        -- OS assigned port from the bootstrap node during startup and inject it
        -- into the configuration of the remaining nodes.
    let baseConf = defaultChainwebConfiguration v

    liftIO $ forConcurrently_ (zip [0..] confBuilders) $ \(i, confBuilder) -> do
        threadDelay (100_000 * int i)

        conf <- if
            | i == 0 ->
                return $ multiBootstrapConfig baseConf
            | otherwise ->
                setBootstrapPeerInfo <$> readMVar bootstrapPortVar <*> pure baseConf

        multiNode loglevel bootstrapPortVar (confBuilder conf) sched rdb pactDbDir (NodeId i) inner

runNodesForSeconds
    :: LogLevel
        -- ^ Loglevel
    -> (T.Text -> IO ())
        -- ^ logging backend callback
    -> ChainwebVersion
    -> [ChainwebConfiguration -> ChainwebConfiguration]
    -> Seconds
        -- ^ test duration in seconds
    -> RocksDb
    -> FilePath
    -> (forall logger. NodeId -> StartedChainweb logger -> RocksDb -> IO ())
    -> IO ()
runNodesForSeconds loglevel write v confBuilders (Seconds seconds) rdb pactDbDir inner = do
    void $ timeout (int seconds * 1_000_000)
        $ runNodes loglevel write v confBuilders rdb pactDbDir inner

-- TODO: PP
-- -- | Ensure that we can compact a live node(s).
-- --
-- --   This test works like so:
-- --   1. Run a number of chainweb nodes in a network for 10 seconds.
-- --   2. Ensure that the nodes made sufficient progress.
-- --   3. Run two threads:
-- --        - The first runs all of the nodes for 60 seconds.
-- --        - The second compacts all of the nodes, after sleeping for 5 seconds.
-- --   4. At the end we compare how long each thread took. The node thread should
-- --      outlive the compaction thread.
-- compactLiveNodeTest :: ()
--   => HasVersion
--   => LogLevel
--   -> Natural
--   -> RocksDb
--   -> FilePath
--   -> FilePath
--   -> (String -> IO ())
--   -> IO ()
-- compactLiveNodeTest logLevel n rocksDb srcPactDir targetPactDir step = do
--   let logFun = step . T.unpack
--   let logger = genericLogger logLevel logFun

--   logFun "Phase 1... creating blocks"

--   -- N.B: This consensus state stuff counts the number of blocks
--   -- in RocksDB, rather than the number of blocks in all chains
--   -- on the current cut. This is fine because we ultimately just want
--   -- to make sure that we are making progress (i.e, new blocks).
--   stateVar <- newMVar emptyConsensusState
--   let ct :: Int -> StartedChainweb logger -> IO ()
--       ct = harvestConsensusState logger stateVar
--   do
--     runNodesForSeconds logLevel logFun (multiConfig n) n 10 rocksDb srcPactDir ct
--     Just stats1 <- consensusStateSummary <$> swapMVar stateVar emptyConsensusState
--     assertGe "average block count before proceeding" (Actual $ _statBlockCount stats1) (Expected 50)
--     logFun $ sshow stats1

--   let compactAll = Chronos.stopwatch_ $ do
--         threadDelay 5_000_000
--         Sigma.withDefaultLogger logLevel $ \lgr -> do
--           forM_ [0 .. int @_ @Word n - 1] $ \nid -> do
--             -- We haven't gotten to run the node against the target yet,
--             -- and nothing has created its db directories, so we do that here.
--             createDirectoryIfMissing False (targetPactDir </> show nid)
--             forM_ (allChains v) $ \cid -> do
--               let logger' = addLabel ("nodeId", sshow nid) $ addLabel ("chainId", chainIdToText cid) lgr
--               withSqliteDb cid logger' (srcPactDir </> show nid) False $ \srcDb -> do
--                 withSqliteDb cid logger' (targetPactDir </> show nid) False $ \targetDb -> do
--                   sigmaCompact srcDb targetDb (BlockHeight 25)

--   let run = Chronos.stopwatch_ $ do
--         -- It may seem a bit strange that we never run the node against the
--         -- compacted state (see that we are using 'srcPactDir' here). This is
--         -- because we are only trying to see that compacting a node as it is,
--         -- does not disrupt it.
--         runNodesForSeconds logLevel logFun (multiConfig n) n 60 rocksDb srcPactDir ct
--         Just stats2 <- consensusStateSummary <$> swapMVar stateVar emptyConsensusState
--         assertGe "average block count before proceeding" (Actual $ _statBlockCount stats2) (Expected 100)
--         logFun $ sshow stats2

--   (compactTime, nodeTime) <- concurrently compactAll run

--   assertGe "node runs beyond compaction" (Actual nodeTime) (Expected compactTime)

-- | This test is essentially just calling pact-calc followed by pact-import,
--   and making sure that works end to end.
--
--   For details:
--
--   1. Run a number of chainweb nodes in a network for 10 seconds.
--   2. Ensure that the nodes made sufficient progress.
--   3. For each node, compute its 'ChainGrandHash'es at regularly offset
--      heights (see targetChunkSize and targets in the test for more info)
--   4. Verify that the latest state of consensus has a latest common
--      blockheight which is verifiable against the computed 'ChainGrandHash'es.
--      See 'GrandHash.Import.pactVerify' for the conditions of verifiability.
--   5. Once we have verified the state against the computed grand hashes, we
--      create a copy of the pact database, and drop any state in the copy past
--      the verified 'BlockHeight'.
--   6. Lastly, we compare the state between the two databases (original and
--      copy) at the verified 'BlockHeight'. There should be no differences.
pactImportTest :: ()
  => HasVersion
  => LogLevel
  -> Natural
  -> RocksDb
  -> FilePath
  -> (String -> IO ())
  -> IO ()
pactImportTest logLevel n rocksDb pactDir step = do
  let logFun = step . T.unpack
  let logger = genericLogger logLevel logFun

  logFun "Phase 1... creating blocks"
  logFun $ T.pack pactDir

  -- N.B: This consensus state stuff counts the number of blocks
  -- in RocksDB, rather than the number of blocks in all chains
  -- on the current cut. This is fine because we ultimately just want
  -- to make sure that we are making progress (i.e, new blocks).
  stateVar <- newMVar emptyConsensusState
  let ct :: NodeId -> StartedChainweb logger -> RocksDb -> IO ()
      ct = harvestConsensusState logger stateVar
  runNodesForSeconds logLevel logFun implicitVersion (multiConfigBuilders n) 10 rocksDb pactDir ct
  consensusState <- swapMVar stateVar emptyConsensusState
  Just stats1 <- pure $ consensusStateSummary consensusState
  assertGe "average block count before proceeding" (Actual $ _statBlockCount stats1) (Expected 50)
  logFun $ sshow stats1

  let chains = allChains
  forM_ [0 .. int @_ @Word n - 1] $ \nid -> do
    let logger' = addLabel ("nodeId", sshow nid) logger
    logFunctionText logger' Info $ "Verifying node " <> sshow nid
    let pactNodeDir = pactDir </> show nid
    GrandHash.Utils.withConnections logger' pactNodeDir chains $ \pactConns -> do
      logFunctionText logger' Info "Calculating grand hashes"

      -- Each node is in its own namespace.
      let rdb = rocksDb { _rocksDbNamespace = T.encodeUtf8 $ toText @Word nid }

      latestBlockHeight <- do
        wbhdb <- initWebBlockHeaderDb rdb
        latestCutHeaders <- readHighestCutHeaders (\_ _ -> pure ()) wbhdb (cutHashesTable rdb)
        pure $ maximum $ fmap (view blockHeight) latestCutHeaders

      let targetChunkSize :: BlockHeight
          targetChunkSize = nextLowestPowerOfTen latestBlockHeight
          -- eg if block count is 5343, this will produce [5000, 4000, 3000, 2000, 1000]
          -- eg if block count is 345, this will produce [300, 200, 100]
      let targets :: Set BlockHeight
          targets =
            Set.fromList
            $ L.unfoldr
                (\t -> if t >= targetChunkSize then Just (t, t - targetChunkSize) else Nothing)
                (latestBlockHeight - latestBlockHeight `mod` targetChunkSize)

      grands <- GrandHash.Calc.pactCalc logger' pactConns rdb (GrandHash.Calc.TargetAll targets)

      logFunctionText logger' Info "Verifying state"
      snapshot@(snapshotBlockHeight, snapshotHashes) <- GrandHash.Import.pactVerify logger' pactConns rdb grands
      logFunctionText logger' Debug $ "SNAPSHOT BLOCKHEIGHT = " <> sshow (fst snapshot)
      logFunctionText logger' Debug $ "SNAPSHOT HASHES = " <> sshow (HM.map (\s -> (T.decodeUtf8 (Base16.encode (getChainGrandHash s.pactHash)), view blockHeight s.blockHeader)) (snd snapshot))

      logFunctionText logger' Info "Making a copy of the pact state, and dropping the post-verified content"
      withSystemTempDirectory "pact-copy" $ \copyPactDir -> do
        let copyNodeDir = copyPactDir </> show nid
        GrandHash.Import.pactDropPostVerified logger' pactNodeDir copyNodeDir snapshotBlockHeight snapshotHashes

        GrandHash.Utils.withConnections logger' copyNodeDir chains $ \pactCopyConns -> do
          logFunctionText logger' Info "Checking latest block heights on copy to make sure they match verified state"
          forM_ chains $ \cid -> do
            let db = pactCopyConns ^?! ix cid
            latestBH <- getLatestBlockHeight db
            logFunctionText (addChainIdLabel cid logger') Debug $ "Latest blockheight is " <> sshow latestBH
            assertEqual "Latest blockheight matches verified state" snapshotBlockHeight latestBH

          logFunctionText logger' Info "Diffing state at verified height across both original and copy"
          forM_ chains $ \cid -> do
            let log :: LogLevel -> Text -> IO ()
                log = logFunctionText
                  (addLabel ("snapshotHeight", sshow snapshotBlockHeight) $ addChainIdLabel cid logger')
            srcStateAt <- do
              let db = pactConns ^?! ix cid
              getPactStateAtDiffable db snapshotBlockHeight
            tgtStateAt <- do
              let db = pactCopyConns ^?! ix cid
              getPactStateAtDiffable db snapshotBlockHeight
            let stateDiff = Map.filter (not . Patience.isSame) (Patience.diff srcStateAt tgtStateAt)
            unless (null stateDiff) $ do
              forM_ (Map.toList stateDiff) $ \(tbl, delta) -> do
                case delta of
                  Patience.Same _ -> do
                    pure ()
                  Patience.Old _ -> do
                    log Error $ "Table " <> tbl <> " appeared in the source pact db, but does not appear in the target."
                  Patience.New _ -> do
                    log Error $ "Table " <> tbl <> " appeared in the target pact db, but not appear in the source."
                  Patience.Delta x1 x2 -> do
                    log Error $ "Difference exists between source and target on table " <> tbl <> "."
                    forM_ (Map.filter (not . Patience.isSame) (Patience.diff x1 x2)) $ \case
                      Patience.Same _ -> do
                        pure ()
                      Patience.Old x -> do
                        log Error $ "In table " <> tbl <> ", rowkey " <> T.decodeUtf8 x <> " exists on source but not target."
                      Patience.New x -> do
                        log Error $ "In table " <> tbl <> ", rowkey " <> T.decodeUtf8 x <> " exists on target but not source."
                      Patience.Delta x _y -> do
                        log Error $ "In table " <> tbl <> ", there was a difference in rowdata at rowkey " <> T.decodeUtf8 x <> "."
              assertFailure "Diff failed"

-- TODO: PP
-- -- | Run nodes
-- --   Each node creates blocks
-- --   We wait until they've made a sufficient amount of blocks
-- --   We stop the nodes
-- --   We compact the databases of each node
-- --   We restart all nodes with the same database dirs
-- --   We observe that they can make progress
-- compactAndResumeTest :: ()
--   => HasVersion
--   => LogLevel
--   -> Natural
--   -> RocksDb
--   -> RocksDb
--   -> FilePath
--   -> FilePath
--   -> (String -> IO ())
--   -> IO ()
-- compactAndResumeTest logLevel n srcRocksDb targetRocksDb srcPactDir targetPactDir step = do
--     let logFun = step . T.unpack
--     let logger = genericLogger logLevel logFun

--     logFun "phase 1... creating blocks"
--     -- N.B: This consensus state stuff counts the number of blocks
--     -- in RocksDB, rather than the number of blocks in all chains
--     -- on the current cut. This is fine because we ultimately just want
--     -- to make sure that we are making progress (i.e, new blocks).
--     stateVar <- newMVar emptyConsensusState
--     let ct :: Int -> StartedChainweb logger -> IO ()
--         ct = harvestConsensusState logger stateVar
--     runNodesForSeconds logLevel logFun (multiConfig n) n 10 srcRocksDb srcPactDir ct
--     Just stats1 <- consensusStateSummary <$> swapMVar stateVar emptyConsensusState
--     assertGe "average block count before compaction" (Actual $ _statBlockCount stats1) (Expected 50)
--     logFun $ sshow stats1

--     logFun "phase 2... compacting"

--     logFun "phase 2.1...compacting pact state"
--     forM_ [0 .. int @_ @Word n - 1] $ \nid -> do
--       forM_ (allChains v) $ \cid -> do
--         let logger' = addLabel ("nodeId", sshow nid) $ addLabel ("chainId", chainIdToText cid) logger
--         withSqliteDb cid logger' (srcPactDir </> show nid) False $ \srcDb -> do
--           withSqliteDb cid logger' (targetPactDir </> show nid) False $ \targetDb -> do
--             sigmaCompact srcDb targetDb (BlockHeight 25)

--     logFun "phase 2.2...compacting RocksDB"
--     forM_ [0 .. int @_ @Word n - 1] $ \nid -> do
--       let srcRdb = srcRocksDb { _rocksDbNamespace = T.encodeUtf8 (toText nid) }
--       let tgtRdb = targetRocksDb { _rocksDbNamespace = T.encodeUtf8 (toText nid) }
--       Sigma.doCompactRocksDb (addLabel ("nodeId", sshow nid) logger) allChains 25 srcRdb tgtRdb

--     logFun "phase 3... restarting nodes and ensuring progress"
--     runNodesForSeconds logLevel logFun (multiConfig n) { _configFullHistoricPactState = False } n 10 targetRocksDb targetPactDir ct
--     Just stats2 <- consensusStateSummary <$> swapMVar stateVar emptyConsensusState
--     -- We ensure that we've gotten to at least 1.5x the previous block count
--     assertGe "average block count post-compaction" (Actual $ _statBlockCount stats2) (Expected (3 * _statBlockCount stats1 `div` 2))
--     logFun $ sshow stats2

replayTest
    :: HasVersion
    => LogLevel
    -> Natural
    -> RocksDb
    -> FilePath
    -> (String -> IO ())
    -> IO ()
replayTest loglevel n rdb pactDbDir step = do
        let tastylog = step . T.unpack
        let logFun = step . T.unpack
        tastylog "phase 1..."
        stateVar <- newMVar emptyConsensusState
        let ct = harvestConsensusState (genericLogger loglevel logFun) stateVar
        runNodesForSeconds loglevel logFun implicitVersion (multiConfigBuilders n) 60 rdb pactDbDir ct
        Just stats1 <- consensusStateSummary <$> swapMVar stateVar emptyConsensusState
        assertGe "maximum cut height before reset" (Actual $ _statMaxHeight stats1) (Expected 10)
        tastylog $ sshow stats1
        tastylog "phase 3... replaying"
        let replayInitialHeight = 5
        firstReplayCompleteRef <- newIORef False
        runNodesForSeconds loglevel logFun implicitVersion
            (fmap (set (configCuts . cutInitialBlockHeightLimit) (Just replayInitialHeight) .) (multiConfigBuilders n))
            (Seconds 20) rdb pactDbDir $ \_ cw _ -> case cw of
                RewoundToCut rewoundToCut -> do
                    writeIORef firstReplayCompleteRef True
                    _ <- flip HM.traverseWithKey (_cutMap rewoundToCut) $ \cid bh ->
                        assertEqual ("lower chain " <> sshow cid) replayInitialHeight (view blockHeight bh)
                    return ()
                _ -> error "replayTest: not a replay"
        assertEqual "replay completion" True =<< readIORef firstReplayCompleteRef
        runNodesForSeconds loglevel logFun implicitVersion (multiConfigBuilders n) 60 rdb pactDbDir ct
        Just stats2 <- consensusStateSummary <$> swapMVar stateVar emptyConsensusState
        assertGe "maximum cut height after reset" (Actual $ _statMaxHeight stats2) (Expected $ _statMaxHeight stats1)
        assertLe "maximum cut height after reset" (Actual $ _statMaxHeight stats2) (Expected $ round @Double (int (_statMaxHeight stats1) * 2.1))
        tastylog "done."

-- -------------------------------------------------------------------------- --
-- Test that block production is efficient. The block delay in this test is
-- random with a mean of 1 second; we expect to produce *around* a block a second.

test
    :: HasVersion
    => LogLevel
    -> Natural
    -> Seconds
    -> RocksDb
    -> FilePath
    -> (String -> IO ())
    -> IO ()
test loglevel n seconds rdb pactDbDir step = do
    -- Count log messages and only print the first 60 messages
    let tastylog = step . T.unpack
    let logFun = T.putStrLn
        maxLogMsgs = 60
    var <- newMVar (0 :: Int)
    let countedLog msg = modifyMVar_ var $ \c -> force (succ c) <$
            when (c < maxLogMsgs) (logFun msg)
    stateVar <- newMVar emptyConsensusState
    runNodesForSeconds loglevel countedLog implicitVersion (multiConfigBuilders n) seconds rdb pactDbDir
        (harvestConsensusState (genericLogger loglevel logFun) stateVar)
    state <- readMVar stateVar
    case consensusStateSummary state of
        Nothing -> assertFailure "chainweb didn't make any progress"
        Just stats -> do
            logsCount <- readMVar var
            tastylog $ "Number of logs: " <> sshow logsCount
            tastylog $ "Expected BlockCount: " <> sshow (expectedBlockCount seconds) -- 80 + 19.5 * 20
            tastylog $ encodeToText stats
            tastylog $ encodeToText $ object
                [ "maxEfficiency%" .= (realToFrac (bc $ _statMaxHeight stats) * (100 :: Double) / int (_statBlockCount stats))
                , "minEfficiency%" .= (realToFrac (bc $ _statMinHeight stats) * (100 :: Double) / int (_statBlockCount stats))
                , "medEfficiency%" .= (realToFrac (bc $ _statMedHeight stats) * (100 :: Double) / int (_statBlockCount stats))
                , "avgEfficiency%" .= (realToFrac (bc $ round (_statAvgHeight stats)) * (100 :: Double) / int (_statBlockCount stats))
                ]

            let (orphans, canonical) = orphanStats state
            tastylog $ encodeToText $ object
                [ "canonicalHeaders" .= canonical
                , "orphanHeaders" .= orphans
                , "orphanRate%" .= (int orphans * (100 :: Double) / int (orphans + canonical))
                ]
            forM_ (cutSyncStats state) $ \v -> tastylog $ encodeToText v

            (assertGe "number of blocks") (Actual $ _statBlockCount stats) (Expected $ _statBlockCount l)
            (assertLe "max stored cut count")
                (Actual $ _statMaxCutCount stats)
                (Expected $ ceiling (avgBlockHeightAtCutHeight $ _statMaxHeight stats)
                    + int (diameterAtCutHeight (_statMaxHeight stats)))
            (assertGe "maximum cut height") (Actual $ _statMaxHeight stats) (Expected $ _statMaxHeight l)
            (assertGe "minimum cut height") (Actual $ _statMinHeight stats) (Expected $ _statMinHeight l)
            (assertGe "median cut height") (Actual $ _statMedHeight stats) (Expected $ _statMedHeight l)
            (assertGe "average cut height") (Actual $ _statAvgHeight stats) (Expected $ _statAvgHeight l)

            (assertLe "maximum cut height") (Actual $ _statMaxHeight stats) (Expected $ _statMaxHeight u)
            (assertLe "minimum cut height") (Actual $ _statMinHeight stats) (Expected $ _statMinHeight u)
            (assertLe "median cut height") (Actual $ _statMedHeight stats) (Expected $ _statMedHeight u)
            (assertLe "average cut height") (Actual $ _statAvgHeight stats) (Expected $ _statAvgHeight u)

  where
    l = lowerStats seconds
    u = upperStats seconds

    bc x = blockCountAtCutHeight x - order (chainGraphAtCutHeight x)

-- -------------------------------------------------------------------------- --
-- Results

data ConsensusState = ConsensusState
    { _stateBlockHashes :: !(HS.HashSet BlockHash)
        -- ^ for short tests this is fine. For larger test runs we should
        -- use HyperLogLog+

    , _stateCutMap :: !(HM.HashMap NodeId (Int, Cut))
        -- ^ The latest cut in each node, as well as the number of cuts stored

    , _stateHeightCounts :: !(HM.HashMap (ChainId, BlockHeight) Int)
        -- ^ number of distinct block headers observed at each (chain, height),
        -- for orphan rate analysis

    , _stateCutTrace :: !(HM.HashMap NodeId [(Double, CutHeight)])
        -- ^ (sample time in POSIX seconds, cut height) samples taken during
        -- the run, for cut sync latency analysis
    }
    deriving (Show, Generic, NFData)

emptyConsensusState :: ConsensusState
emptyConsensusState = ConsensusState mempty mempty mempty mempty

sampleConsensusState
    :: HasVersion
    => NodeId
        -- ^ node Id
    -> WebBlockHeaderDb
    -> CutDb l
    -> Casify RocksDbTable CutHashes
    -> ConsensusState
    -> IO ConsensusState
sampleConsensusState nid bhdb cutdb ct s = do
    numStoredCuts <- withTableIterator ct $ \i -> iterToEntryStream i & S.length_
    (!hashes', !heightCounts') <- webEntries bhdb Nothing Nothing
        $ S.fold_
            (\(hs, m) bh ->
                let h = view blockHash bh
                in if HS.member h hs
                    then (hs, m)
                    else (HS.insert h hs, HM.insertWith (+) (_chainId bh, view blockHeight bh) 1 m))
            (_stateBlockHashes s, _stateHeightCounts s)
            id
    !c <- _cut cutdb
    return $! s
        { _stateBlockHashes = hashes'
        , _stateCutMap = HM.insert nid (numStoredCuts, c) (_stateCutMap s)
        , _stateHeightCounts = heightCounts'
        }

data Stats = Stats
    { _statBlockCount :: !Natural
    , _statMaxHeight :: !CutHeight
    , _statMinHeight :: !CutHeight
    , _statMedHeight :: !CutHeight
    , _statAvgHeight :: !Double
    , _statMaxCutCount :: Int
    }
    deriving (Show, Eq, Ord, Generic, ToJSON)
instance J.Encode Stats where
    build = J.build . J.encodeWithAeson

consensusStateSummary :: HasVersion => ConsensusState -> Maybe Stats
consensusStateSummary s
    | HM.null (_stateCutMap s) = Nothing
    | otherwise = Just Stats
        { _statBlockCount = int hashCount
        , _statMaxHeight = maxHeight
        , _statMinHeight = minHeight
        , _statMedHeight = medHeight
        , _statAvgHeight = avgHeight
        , _statMaxCutCount = fst $ maximumBy (compare `on` fst) $ _stateCutMap s
        }
  where
    cutHeights = _cutHeight . snd <$> _stateCutMap s
    graph = chainGraphAt
        $ maximum . concatMap (chainHeights . snd)
        $ toList
        $ _stateCutMap s
    hashCount = HS.size (_stateBlockHashes s) - int (order graph)

    avg :: Foldable f => Real a => f a -> Double
    avg f = realToFrac (sum $ toList f) / realToFrac (length f)

    median :: (Ord a, Integral a) => (Foldable f) => f a -> a
    median f = case L.sort (toList f) of
      [] -> error "median: empty list"
      xs | length xs `mod` 2 == 1 -> xs !! (length f `div` 2)
         | otherwise -> ((xs !! (length xs `div` 2 - 1)) + (xs !! (length xs `div` 2))) `div` 2

    minHeight = minimum $ HM.elems cutHeights
    maxHeight = maximum $ HM.elems cutHeights
    avgHeight = avg $ HM.elems cutHeights
    medHeight = median $ HM.elems cutHeights

expectedBlockCount :: HasVersion => Seconds -> Double
expectedBlockCount s = expectedGlobalBlockCountAfterSeconds s

-- | Count canonical vs orphan headers relative to the highest cut observed in
-- any node. A header is canonical if its (chain, height) is at or below the
-- height of that chain in the best cut; all other headers at canonical heights
-- are orphans.
orphanStats :: ConsensusState -> (Int, Int)
orphanStats s = L.foldl' step (0, 0) (HM.toList (_stateHeightCounts s))
  where
    bestCut = snd $ maximumBy (compare `on` _cutHeight . snd) $ HM.elems (_stateCutMap s)
    maxHeights = HM.map (view blockHeight) (_cutMap bestCut)
    step (orphans, canonical) ((cid, h), k)
        | Just h' <- HM.lookup cid maxHeights, h <= h'
            = (orphans + k - 1, canonical + 1)
        | otherwise = (orphans, canonical)

-- | Measure cut sync latency from per-node cut height traces: for each cut
-- height, how long after the first node reached it did a typical (median)
-- node reach it, and how long did the last node take. Heights in the initial
-- startup transient (below a quarter of the commonly reached height) are
-- excluded.
cutSyncStats :: ConsensusState -> Maybe Value
cutSyncStats s
    | length reachMaps < 2 = Nothing
    | null lags = Nothing
    | otherwise = Just $ object
        [ "medianSyncMs" .= pct 0.5 (map fst3 lags)
        , "p90SyncMs" .= pct 0.9 (map fst3 lags)
        , "maxSyncMs" .= pct 0.5 (map thd3 lags)
        , "heightsMeasured" .= length lags
        ]
  where
    reachMaps = map reach (HM.elems (_stateCutTrace s))
      where
        -- first time this node reached each height; samples are in time order
        reach = go 0 . L.sortOn fst
          where
            go _ [] = []
            go prev ((t, h):rest)
                | h <= prev = go prev rest
                | otherwise = [(ht, t) | ht <- [prev + 1 .. h]] ++ go h rest
    hi = minimum (map (fst . last) reachMaps)
    lo = hi `div` 4
    lags :: [(Int, Int, Int)]
    lags = L.sort
        [ ( round $ 1000 * (med - mn)
          , round $ 1000 * (p90 - mn)
          , round $ 1000 * (mx - mn)
          )
        | h <- [lo .. hi]
        , let ts = L.sort [t | m <- reachMaps, Just t <- [L.lookup h m]]
        , length ts == length reachMaps
        , let mn = head ts
              mx = last ts
              med = pct 0.5 ts
              p90 = pct 0.9 ts
        ]
    fst3 (a,_,_) = a
    thd3 (_,_,c) = c
    pct p xs = xs !! min (length xs - 1) (floor (p * int (length xs - 1)))

lowerStats :: HasVersion => Seconds -> Stats
lowerStats seconds = Stats
    { _statBlockCount = round $ ebc * 0.6 -- temporarily, was 0.8
    , _statMaxHeight = round $ ebc * 0.1 -- temporarily, was 0.7
    , _statMinHeight = round $ ebc * 0.05 -- temporarily, was 0.3
    , _statMedHeight = round $ ebc * 0.1
    , _statAvgHeight = ebc * 0.1
    , _statMaxCutCount = undefined
    }
  where
    ebc = expectedBlockCount seconds


upperStats :: HasVersion => Seconds -> Stats
upperStats seconds = Stats
    { _statBlockCount = round $ ebc * 1.4
    , _statMaxHeight = round $ ech * 1.4
    , _statMinHeight = round $ ech * 1.4
    , _statMedHeight = round $ ech * 1.4
    , _statAvgHeight = ech * 1.4
    , _statMaxCutCount = undefined
    }
  where
    ebc = expectedBlockCount seconds
    ech = expectedCutHeightAfterSeconds seconds

nextLowestPowerOfTen :: forall a. (Integral a, Ord a) => a -> a
nextLowestPowerOfTen n
    | n <= 0    = 0
    | isPowerOf10 n = n
    | otherwise = 10 ^ floor @Double @a (logBase 10 (fromIntegral n))

isPowerOf10 :: (Integral a, Ord a) => a -> Bool
isPowerOf10 n = n > 0 && (n == 1 || (n > 1 && n `mod` 10 == 0 && isPowerOf10 (n `div` 10)))

getPactStateAtDiffable :: Database -> BlockHeight -> IO (Map Text (Map ByteString ByteString))
getPactStateAtDiffable db bh = do
  S.foldM_
    (\m td -> pure (Map.insert td.name td.rows m))
    (pure Map.empty)
    pure
    (getLatestPactStateAtDiffable db bh)
