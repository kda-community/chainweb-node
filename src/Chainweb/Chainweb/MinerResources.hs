{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module: Chainweb.Chainweb.MinerResources
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Resources for initializing mining and related components.
--
-- This datastructure must only be used during node startup. No heap reference
-- should be kept after intialization of the node is complete.
--
module Chainweb.Chainweb.MinerResources
  ( -- * In-process Mining
    MinerResources(..)
  , withMinerResources
  , runMiner
    -- * Remote Work Requests
  , MiningCoordination(..)
  , withMiningCoordination
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar
import Control.Exception (finally)
import Control.Lens
import Control.Monad

import Data.HashMap.Strict (HashMap)
import Data.Coerce (coerce)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.IORef (IORef, atomicWriteIORef, newIORef, readIORef)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector as V

import System.LogLevel (LogLevel(..))
import qualified System.Random.MWC as MWC

-- internal modules

import Chainweb.BlockHeader
import Chainweb.ChainId
import Chainweb.BlockHash
import Chainweb.Chainweb.ChainResources
import Chainweb.CutDB (CutDb, awaitNewBlock, cutDbPactService)
import Chainweb.Logger
import Chainweb.Miner.Config
import Chainweb.Miner.Coordinator
import Chainweb.Miner.Miners
import Chainweb.Miner.Pact (Miner(..), minerId, MinerId(..))
import Chainweb.Pact.Types
import Chainweb.Pact.Utils
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.Sync.WebBlockHeaderStore
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Version
import Chainweb.WebPactExecutionService
import Utils.Logging.Trace

import Data.LogMessage (JsonLog(..), LogFunction)

import Numeric.AffineSpace

-- -------------------------------------------------------------------------- --
-- Miner

data MinerLastSeen = NeverExpire | LastSeen (Time Micros)
                   --    ^              ^-- For Dynamic miners
                   --     \ For static mineres
    deriving (Eq)

data ActiveMiner = ActiveMiner
    { _lastSeen :: MinerLastSeen
    , _handles  :: [Async ()]
    }

makeLenses ''ActiveMiner

isMinerExpired:: Time Micros -> ActiveMiner -> Bool
isMinerExpired exp_t miner = case (miner ^. lastSeen) of
                                NeverExpire -> False
                                LastSeen t | t < exp_t -> True
                                LastSeen _ -> False


type ActiveMiners = HM.HashMap MinerId ActiveMiner

-- Return all Handles from an ActiveMiners HashMap
allHandles :: ActiveMiners -> [Async ()]
allHandles = mconcat . fmap (^. handles) . HM.elems

withMiningCoordination
    :: Logger logger
    => logger
    -> MiningConfig
    -> CutDb tbl
    -> (Maybe (MiningCoordination logger tbl) -> IO a)
    -> IO a
withMiningCoordination logger conf cdb inner
    | not (_coordinationEnabled coordConf) = inner Nothing
    | otherwise = do

        t <- newTVarIO mempty
        tpw <- newTVarIO (PrimedWork HM.empty)
        tam <- newTVarIO HM.empty

        let miners = S.toList (_coordinationMiners coordConf)
                     <> [ _nodeMiner inNodeConf | _nodeMiningEnabled inNodeConf ]

        forM_ miners $ initMiner tam tpw

        c503 <- newIORef 0
        c403 <- newIORef 0
        l <- newIORef (_coordinationUpdateStreamLimit coordConf)
        withAsync (prune t tam tpw c503 c403) $ \_ -> do
            inner (Just $ MiningCoordination
                { _coordLogger = logger
                , _coordCutDb = cdb
                , _coordState = t
                , _coordLimit = _coordinationReqLimit coordConf
                , _coord503s = c503
                , _coord403s = c403
                , _coordConf = coordConf
                , _coordUpdateStreamCount = l
                , _coordRefreshMiner = updateDynamicMiner tam tpw
                , _coordPrimedWork = tpw
                , _coordTargetFork =
                    if _coordinationTargetForkOverride coordConf
                    then pred $ max 1 (_versionForkNumber v)
                    else _versionForkNumber v
                })

            `finally` do
                am <- readTVarIO tam
                cancelMany $ allHandles am
  where
    coordConf = _miningCoordination conf
    inNodeConf = _miningInNode conf
    v = _chainwebVersion cdb

    cids :: [ChainId]
    cids = HS.toList (chainIds v)

    chainLogger cid = addLabel ("chain", toText cid)

    minerIdLogger:: Logger l => MinerId -> (l -> l)
    minerIdLogger mid = addLabel ("miner", _minerId mid)

    updateDynamicMiner :: TVar ActiveMiners ->  TVar PrimedWork -> Miner -> IO (Either String ())
    updateDynamicMiner tam tpm m = do
        let mid = view minerId m
        ct <- getCurrentTimeIntegral
        isNewMiner <- atomically $ do
            am <- readTVar tam
            case HM.lookup mid am of
                -- Static Miner
                Just currentMiner | currentMiner ^. lastSeen == NeverExpire -> return (Right False)

                -- Dynamic miner already registered, update the time
                Just _ -> do
                    modifyTVar tam (HM.adjust (lastSeen .~ LastSeen ct) mid)
                    return (Right False)

                -- Dynamic miners no atllowed
                Nothing | not (_coordinationDynamicMinersEnabled coordConf) -> return ( Left "Unknown miners not allowed")

                -- Miner doesn't exist but we are full
                Nothing | HM.size am > (_coordinationMinersLimit coordConf) -> return ( Left "Miners Pool Full")

                -- Miner doesn't exist but there is some room for it
                Nothing -> do
                    -- Insert into active miners list
                    modifyTVar tam (HM.insert mid (ActiveMiner (LastSeen ct) []))
                    -- And prepare a PrimeWork structure with evrything Stale, and let updatePrimeWork take care of it
                    modifyTVar tpm  $ coerce $ HM.insert mid (HM.fromList $ fmap (\c -> (c, WorkStale)) cids)
                    return $ Right True

        -- In case the miner is new => Start the updatePrimeWork threads
        forM isNewMiner $ \isNew ->
            when isNew $ do
                logFunctionText (minerIdLogger mid logger) Info "Added"
                -- Create the threads
                h <- mapM (async . updatePrimeWork tpm m) cids
                -- And store their handles to cancel them in case the miner stops.
                atomically $ modifyTVar tam (HM.adjust (handles .~ h) mid)

    -- Only used for static miners
    -- A smplified version of updateDynamic miner
    initMiner :: TVar ActiveMiners ->  TVar PrimedWork -> Miner -> IO()
    initMiner tam tpm m = do
        let mid = view minerId m
        atomically $ do
            modifyTVar tam (HM.insert mid (ActiveMiner NeverExpire []))
            modifyTVar tpm  $ coerce $ HM.insert mid (HM.fromList $ fmap (\c -> (c, WorkStale)) cids)

        h <- mapM (async . updatePrimeWork tpm m) cids
        atomically $ modifyTVar tam (HM.adjust (handles .~ h) mid)

    -- we assume that this path always exists in PrimedWork and never delete it.
    workForMiner :: Miner -> ChainId -> Traversal' PrimedWork WorkState
    workForMiner miner cid = _Wrapped' . ix (view minerId miner) . ix cid

    periodicallyRefreshPayload :: TVar PrimedWork -> ChainId -> Miner -> IO a
    periodicallyRefreshPayload tpw cid ourMiner = forever $ do
        let delay =
                timeSpanToMicros $ _coordinationPayloadRefreshDelay coordConf
        threadDelay (fromIntegral @Micros @Int delay)
        when (not $ v ^. versionCheats . disablePact) $ do
            -- "stale" in the sense of not having all of the transactions
            -- that it could. it still has the latest possible parent
            mContinuableBlockInProgress <- atomically $ do
                primed <- readTVar tpw <&> (^?! workForMiner ourMiner cid)
                case primed of
                    WorkReady (NewBlockInProgress bip) -> return (Just bip)
                    WorkReady (NewBlockPayload {}) ->
                        error "periodicallyRefreshPayload: encountered NewBlockPayload in PrimedWork, which cannot be refreshed"
                    WorkAlreadyMined {} -> return Nothing
                    WorkStale -> return Nothing

            forM_ mContinuableBlockInProgress $ \continuableBlockInProgress -> do
                maybeNewBlock <- case continuableBlockInProgress of
                    ForPact4 block -> fmap ForPact4 <$> _pactContinueBlock pact cid block
                    ForPact5 block -> fmap ForPact5 <$> _pactContinueBlock pact cid block
                -- if continuing returns NoHistory then the parent header
                -- isn't available in the checkpointer right now.
                -- in that case we just mark the payload as not stale.
                let newBlock = case maybeNewBlock of
                        NoHistory -> continuableBlockInProgress
                        Historical b -> b

                logFunctionText (chainLogger cid logger) Debug
                    $ "refreshed block, old and new tx count: "
                    <> sshow
                        ( forAnyPactVersion (V.length . _transactionPairs . _blockInProgressTransactions) continuableBlockInProgress
                        , forAnyPactVersion (V.length . _transactionPairs . _blockInProgressTransactions) newBlock
                        )

                atomically $ modifyTVar' tpw $
                    workForMiner ourMiner cid .~ WorkReady (NewBlockInProgress newBlock)

    -- | THREAD: Keep a live-updated cache of Payloads for specific miners, such
    -- that when they request new work, the block can be instantly constructed
    -- without interacting with the Pact Queue.
    --
    updatePrimeWork :: TVar PrimedWork -> Miner -> ChainId -> IO ()
    updatePrimeWork tpw miner cid =  runForever (logFunction (chainLogger cid logger)) "primeWork" go
        where
        go :: IO ()
        go = do
            let ourMiner :: Traversal' PrimedWork WorkState
                ourMiner = workForMiner miner cid

            pw <- readTVarIO tpw

            outdatedParentHash <- case pw ^? ourMiner of
                    Just (WorkReady outdatedBlock) -> return $ view _1 (newBlockParent outdatedBlock)
                    Just (WorkAlreadyMined outdatedBlockHash) -> return outdatedBlockHash
                    -- Stale here is not an usual state (newly registered miners, or failure during previous iteration)
                    -- => using nullBlockHash will uncondionally trigger awaitNewBlock
                    Just WorkStale -> return nullBlockHash
                    -- Having no primerwork data is not normal. It could only happen in a race condition when the thread is going to be cancelled
                    -- Better we can do is to wait for our death, it should happen very soon.
                    Nothing -> forever (threadDelay 10_000_000) >> return nullBlockHash

            newParent <- either ParentHeader id <$> race
                -- wait for a block different from what we've got primed work for
                (awaitNewBlock cdb cid outdatedParentHash)
                -- in the meantime, periodically refresh the payload to make sure
                -- it has all of the transactions it can have
                (periodicallyRefreshPayload tpw cid miner)

            -- Temporarily block this chain from being considered for queries
            atomically $ modifyTVar' tpw (ourMiner .~ WorkStale)

            -- Get a payload for the new block
            getPayload cid miner newParent >>= \case
                NoHistory -> do
                    logFunctionText (addLabel ("chain", toText cid) logger) Warn
                        "current block is not in the checkpointer; halting primed work loop temporarily"
                    approximateThreadDelay 1_000_000
                Historical newBlock ->
                    atomically $ modifyTVar' tpw (ourMiner .~ WorkReady newBlock)

    getPayload :: ChainId -> Miner -> ParentHeader -> IO (Historical NewBlock)
    getPayload cid m ph =
        if v ^. versionCheats . disablePact
        -- if pact is disabled, we must keep track of the latest header
        -- ourselves. otherwise we use the header we get from newBlock as the
        -- real parent. newBlock may return a header in the past due to a race
        -- with rocksdb though that shouldn't cause a problem, just wasted work,
        -- see docs for
        -- Chainweb.Pact.PactService.Checkpointer.findLatestValidBlockHeader'
        then return $ Historical $
            NewBlockPayload ph emptyPayload
        else trace (logFunction (chainLogger cid logger))
            "Chainweb.Chainweb.MinerResources.withMiningCoordination.newBlock"
            () 1 (_pactNewBlock pact cid m NewBlockFill ph)

    pact :: PactExecutionService
    pact = _webPactExecutionService $ view cutDbPactService cdb

    -- | THREAD: Periodically clear out the cached payloads kept for Mining
    -- Coordination.
    --
    prune :: TVar MiningState -> TVar ActiveMiners -> TVar PrimedWork -> IORef Int -> IORef Int -> IO ()
    prune t tam tpw c503 c403 = runForever (logFunction logger) "MinerResources.prune" $ do
        let !d = 30_000_000  -- 30 seconds
        let !maxAge = (5 :: Int) `scaleTimeSpan` minute -- 5 minutes
        threadDelay d
        ago <- (.-^ maxAge) <$> getCurrentTimeIntegral
        m@(MiningState ms) <- atomically $ do
            ms <- readTVar t
            modifyTVar' t . over miningState $ M.filter (f ago)
            pure ms

        -- Remove dynamic miners not seen since more than 5 minutes
        removedMiners <- atomically $ do
            am <- readTVar tam
            let expiredMiners = HM.filter (isMinerExpired ago) am
            forM_ (HM.keys expiredMiners) $ \mid -> do
                modifyTVar tam $ HM.delete mid
                modifyTVar tpw $ coerce $ PrimedWork . HM.delete mid
            return expiredMiners

        cancelMany $ allHandles removedMiners

        forM_ (HM.keys removedMiners) $ \mid -> logFunctionText (minerIdLogger mid logger) Info "Nos seen since a while => Removed"

        count503 <- readIORef c503
        count403 <- readIORef c403
        PrimedWork pw <- readTVarIO tpw
        atomicWriteIORef c503 0
        atomicWriteIORef c403 0
        logFunction logger Info . JsonLog $ MiningStats
            { _statsCacheSize = M.size ms
            , _stats503s = count503
            , _stats403s = count403
            , _statsAvgTxs = avgTxs m
            , _statsPrimedSize = HM.foldl' (\acc xs -> acc + HM.size xs) 0 pw }

    -- Filter for work items that are not older than maxAge
    --
    -- NOTE: Should difficulty ever become that hard that five minutes aren't
    -- sufficient to mine a block this constant must be changed in order to
    -- recover.
    --
    f :: Time Micros -> T3 a b (Time Micros) -> Bool
    f ago (T3 _ _ added) = added > ago

    avgTxs :: MiningState -> Int
    avgTxs (MiningState ms) = summed `div` max 1 (M.size ms)
      where
        summed :: Int
        summed = M.foldl' (\acc (T3 _ ps _) -> acc + g ps) 0 ms

        g :: PayloadWithOutputs -> Int
        g = V.length . _payloadWithOutputsTransactions

-- | Miner resources are used by the test-miner when in-node mining is
-- configured or by the mempool noop-miner (which keeps the mempool updated) in
-- production setups.
--
data MinerResources logger tbl = MinerResources
    { _minerResLogger :: !logger
    , _minerResCutDb :: !(CutDb tbl)
    , _minerChainResources :: !(HashMap ChainId (ChainResources logger))
    , _minerResConfig :: !NodeMiningConfig
    , _minerResCoordination :: !(Maybe (MiningCoordination logger tbl))
        -- ^ The primed work cache. This is Nothing when coordination is
        -- disabled. It is needed by the in-node test miner. The mempoolNoopMiner
        -- does not use it.
    }

withMinerResources
    :: logger
    -> NodeMiningConfig
    -> HashMap ChainId (ChainResources logger)
    -> CutDb tbl
    -> Maybe (MiningCoordination logger tbl)
    -> (Maybe (MinerResources logger tbl) -> IO a)
    -> IO a
withMinerResources logger conf chainRes cutDb tpw inner =
    inner . Just $ MinerResources
        { _minerResLogger = logger
        , _minerResCutDb = cutDb
        , _minerChainResources = chainRes
        , _minerResConfig = conf
        , _minerResCoordination = tpw
        }

-- | This runs the internal in-node miner. It is only used during testing.
--
-- When mining coordination is disabled, this function exits with an error.
--
runMiner
    :: forall logger tbl
    .  Logger logger
    => CanReadablePayloadCas tbl
    => ChainwebVersion
    -> MinerResources logger tbl
    -> IO ()
runMiner v mr
    | enabled = case _minerResCoordination mr of
        Nothing -> error
            "Mining coordination must be enabled in order to use the in-node test miner"
        Just coord -> case v ^. versionCheats . disablePow of
            True -> testMiner coord
            False -> powMiner coord
    | otherwise = mempoolNoopMiner lf (_chainResMempool <$> _minerChainResources mr)

  where
    enabled = _nodeMiningEnabled $ _minerResConfig mr

    cdb :: CutDb tbl
    cdb = _minerResCutDb mr

    conf :: NodeMiningConfig
    conf = _minerResConfig mr

    lf :: LogFunction
    lf = logFunction $ _minerResLogger mr

    testMiner coord = do
        gen <- MWC.createSystemRandom
        localTest lf v coord (_nodeMiner conf) cdb gen (_nodeTestMiners conf)

    powMiner coord = localPOW lf coord (_nodeMiner conf) cdb
