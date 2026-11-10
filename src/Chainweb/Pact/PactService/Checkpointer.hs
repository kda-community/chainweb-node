{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Chainweb.Pact.PactService.Checkpointer
-- Copyright: Copyright © 2020 Kadena LLC.
-- License: See LICENSE file
-- Maintainers: Stuart Popejoy, Lars Kuhtz, Emily Pillmore
-- Stability: experimental
--
-- Checkpointer interaction for Pact service.
--
module Chainweb.Pact.PactService.Checkpointer
    ( readFromLatest
    , readFromNthParent
    , readFrom
    , restoreAndSave
    , rewindTo
    -- , findLatestValidBlockHeader'
    -- , findLatestValidBlockHeader
    -- , exitOnRewindLimitExceeded
    , getEarliestBlock
    , getLatestBlock
    -- , lookupBlock
    , lookupRankedBlockHash
    , lookupBlockHash
    , lookupBlockWithHeight
    , getPayloadsAfter
    , getConsensusState
    , setConsensusState
    , RunnableBlock(..)
    , PactRead(..)
    , readPact5
    ) where

import Control.Lens hiding ((:>), (:<))
import Control.Monad
import Control.Monad.Reader
import Data.Maybe
import Data.Monoid hiding (Product (..))
import GHC.Stack
import Prelude hiding (lookup)
import Pact.Core.Persistence.Types qualified as Pact
import Chainweb.BlockCreationTime
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeight
import Chainweb.ForkState (ForkNumber)
import Control.Monad.Trans.Maybe
import Chainweb.Logger
import Chainweb.MinerReward
import Chainweb.Pact.Backend.ChainwebPactDb qualified as ChainwebPactDb
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Backend.Utils qualified as Backend.Utils
import Chainweb.Pact.Types
import Chainweb.Parent
import Chainweb.PayloadProvider
import Chainweb.Ranked
import Chainweb.Utils hiding (check)
import Chainweb.Version
import Chainweb.Version.Guards (pact5, minimumBlockHeaderHistory)
import Control.Exception.Safe (MonadMask)
import Data.List.NonEmpty (NonEmpty)
import Chainweb.Pact.Backend.ChainwebPactDb (lookupRankedBlockHash)
import Control.Monad.State.Strict
import System.LogLevel
import Chainweb.Pact4.Backend.ChainwebPactDb qualified as Pact4
import Control.Concurrent.MVar
import Pact.Types.Persistence qualified as Pact4
import Chainweb.Pact.Backend.DbCache (defaultModuleCacheLimit)
import Pact.Interpreter qualified as Pact4
import Data.ByteString.Short qualified as BS
import Data.Coerce
import Data.HashMap.Strict qualified as HashMap
import Pact.Types.Command qualified as Pact4
import Pact.Types.Hash qualified as Pact4
import Data.Foldable1 (foldlMapM1)


-- We try to recover the clock context from the history with a rewind of n.
--
-- We read both the nth block (target) and its parent:
--  - parentBlockCreationTime and ParentForkNumber are extracted from the stored context associated to target
--  - parentHash is extracted from its parent (referenced by Height)
--
-- n=0 is a special case. n=0 means that the context is related to the next mined block
-- The parent block is the tip.. But we still ignore what is his FOrNumber and BlockCreationTime
-- Even if the block is already mined, Consensus layer didn't send us.
-- In this case, we take the ForkNumber and BlockCreationTime from the GrandParent
--
blockCtxFromHistory:: (HasCallStack, HasVersion) => ChainId ->  SQLiteEnv -> Word -> MaybeT IO BlockCtx
blockCtxFromHistory cid sql n = do
    latest@(latestH, _latestFn, _latestCt) <- MaybeT $ latestBlocEnv sql
    (parentH, _parentFn, _parentCt) <- if n==0
                                       then return latest
                                       else MaybeT $ lookupBlocEnvkWithHeight sql $ (_rankedHeight latestH - fromIntegral n)

    (_targetH, targetFn, targetCt) <- if n==0 || n==1
                                      then return latest
                                      else MaybeT $ lookupBlocEnvkWithHeight sql $ (_rankedHeight latestH - fromIntegral n + 1)

    return BlockCtx {_bctxParentCreationTime = targetCt -- TODO Add 30 s in case of n=0, as an estimator ?? There are pros and cons...
                    , _bctxParentHash = Parent $ _ranked parentH
                    , _bctxParentHeight = Parent $ _rankedHeight parentH
                    , _bctxParentForkNumber = unwrapParent targetFn
                    , _bctxChainId = cid
                    , _bctxMinerReward = blockMinerReward (_rankedHeight latestH - fromIntegral n +1)
                    }

-- read-only rewind to the latest block.
-- note: because there is a race between getting the latest header
-- and doing the rewind, there's a chance that the latest header
-- will be unavailable when we do the rewind. in that case
-- we just keep grabbing the new "latest header" until we succeed.
-- note: this function will never rewind before genesis.
readFromLatest
    :: (HasCallStack, HasVersion, Logger logger)
    => logger
    -> ChainId
    -> SQLiteEnv
    -> PactRead a
    -> IO a
readFromLatest logger cid sql doRead =
    readFromNthParent logger cid sql 0 doRead >>= \case
        NoHistory -> error "readFromLatest: failed to grab the latest header, this is a bug in chainweb"
        Historical a -> return a

-- read-only rewind to the nth parent before the latest block.
-- note: this function will never rewind before genesis.
readFromNthParent
    :: (HasCallStack, HasVersion, Logger logger)
    => logger
    -> ChainId
    -> SQLiteEnv
    -> Word
    -> PactRead a
    -> IO (Historical a)
readFromNthParent logger cid sql n doRead = do
    latest <- (_syncStateHeight . fromMaybe (error "readFromNthParent is illegal to call before genesis"))
              <$> (ChainwebPactDb.throwOnDbError $ ChainwebPactDb.getLatestBlock sql)

    if genesisHeight cid + fromIntegral @Word @BlockHeight n > latest --TODO check if a +1 condition is required here
    then do
        logFunctionText logger Warn $ "readFromNthParent asked to rewind beyond genesis, to "
            <> sshow (int latest - int n :: Integer)
        return NoHistory

    else do
        mbctx <- runMaybeT $ blockCtxFromHistory cid sql n
        case mbctx of
            Nothing -> logFunctionText logger Warn "readFromNthParent asked to rewind beyond known blocks" >> return NoHistory
            Just bctx -> readFrom logger cid sql bctx doRead


readFrom
    :: (HasCallStack, HasVersion, Logger logger)
    => logger
    -> ChainId
    -> SQLiteEnv
    -> BlockCtx
    -> PactRead a
    -> IO (Historical a)
readFrom logger cid sql blockCtx pactRead = do
    !latestHeader <- ChainwebPactDb.throwOnDbError
        $ fmap (maybe (genesisRankedParentBlockHash cid) (Parent . _syncStateRankedBlockHash))
            $ ChainwebPactDb.getLatestBlock sql
    -- is the parent the latest header, i.e., can we get away without rewinding?
    let parentIsLatestHeader = latestHeader == _bctxParentRankedBlockHash blockCtx
    let currentHeight = _bctxCurrentBlockHeight blockCtx

    Backend.Utils.getEndTxId cid sql (_bctxParentRankedBlockHash blockCtx) >>= traverse \startTxId ->
        if pact5 cid currentHeight then do
            let
                blockHandlerEnv = ChainwebPactDb.BlockHandlerEnv
                    { ChainwebPactDb._blockHandlerDb = sql
                    , ChainwebPactDb._blockHandlerLogger = logger
                    , ChainwebPactDb._blockHandlerChainId = cid
                    , ChainwebPactDb._blockHandlerBlockHeight = currentHeight
                    , ChainwebPactDb._blockHandlerMode = Pact.Transactional
                    , ChainwebPactDb._blockHandlerUpperBoundTxId = startTxId
                    , ChainwebPactDb._blockHandlerAtTip = parentIsLatestHeader
                    }
            let minOracleHistory = minimumBlockHeaderHistory (blockCtx ^. bctxParentForkNumber) currentHeight
            let pactDb = ChainwebPactDb.chainwebPactBlockDb blockHandlerEnv minOracleHistory
            let blockEnv = BlockEnv blockCtx pactDb
            pact5Read pactRead blockEnv (emptyBlockHandle startTxId)
        else do
            let pact4TxId = Pact4.TxId (coerce startTxId)
            let blockHandlerEnv = Pact4.mkBlockHandlerEnv cid currentHeight sql logger
            newBlockDbEnv <- liftIO $ newMVar $ Pact4.BlockDbEnv
                blockHandlerEnv
                -- FIXME not sharing the cache
                (Pact4.initBlockState defaultModuleCacheLimit pact4TxId)
            let pactDb = Pact4.rewoundPactDb currentHeight pact4TxId

            let pact4DbEnv = Pact4.CurrentBlockDbEnv
                    { _cpPactDbEnv = Pact4.PactDbEnv pactDb newBlockDbEnv
                    , _cpRegisterProcessedTx = \hash ->
                        Pact4.runBlockDbEnv newBlockDbEnv (Pact4.indexPactTransaction $ BS.fromShort $ Pact4.unHash $ Pact4.unRequestKey hash)
                    , _cpLookupProcessedTx = \hs -> do
                        res <- Backend.Utils.doLookupSuccessful sql currentHeight (coerce hs)
                        return
                            $ HashMap.mapKeys coerce
                            $ HashMap.map
                                (\(T3 height _payloadhash bhash) -> T2 height bhash)
                                res
                    , _cpHeaderOracle = Pact4.headerOracleForBlock blockHandlerEnv
                    }
            pact4Read pactRead (Pact4.BlockEnv blockCtx pact4DbEnv)

-- the special case where one doesn't want to extend the chain, just rewind it.
rewindTo
    :: HasVersion
    => ChainId
    -> SQLiteEnv
    -> Parent RankedBlockHash
    -> IO ()
rewindTo cid sql ancestor = do
    void $ Backend.Utils.rewindDbTo cid sql ancestor

data PactRead a
    = PactRead
        { pact4Read :: (forall logger. HasCallStack => Logger logger => Pact4.BlockEnv logger -> IO a)
        , pact5Read :: HasCallStack => BlockEnv -> BlockHandle -> IO a
        }

readPact5 :: String -> (BlockEnv -> BlockHandle -> IO a) -> PactRead a
readPact5 msg act = PactRead
    { pact4Read = \_ -> error msg -- TODO: Not good => possible DoS for a node that is catching up ?
    , pact5Read = act
    }

data RunnableBlock m q
    = Pact4RunnableBlock
        BlockCtx -- <- FOR API consistency, in fact Pact4 doesn't need it
        (forall logger. Logger logger => Pact4.CurrentBlockDbEnv logger -> m (q, (BlockHash, BlockPayloadHash)))
    | Pact5RunnableBlock
        BlockCtx
        (ChainwebPactDb -> StateT BlockHandle m (q, (BlockHash, BlockPayloadHash)))

foldState1
    :: (Monad m, Semigroup a)
    => NonEmpty (s -> m (a, s))
    -> s
    -> m a
foldState1 bs !initialState = do
    fst <$> foldlMapM1 ($ initialState) g bs
    where
    g (a, s) f = do
        (b, s') <- f s
        let !c = a <> b
        pure (c, s')

-- TODO: log more?
-- | Given a list of blocks in ascending order, rewind to the first
-- of them and play all of the subsequent blocks, saving the result persistently.
-- this function takes care of making sure that this is done *atomically*.
-- The two following expressions should be equivalent:
-- restoreAndSave cp v cid blocks1 <> restoreAndSave cp v cid blocks2
-- restoreAndSave cp v cid (blocks1 <> blocks2)
--
-- prerequisites:
--   - the first block's parent must be an ancestor of the latest block in the database.
--   - each subsequent block must be the direct child of the previous block.
restoreAndSave
    :: forall logger m q.
    (Logger logger, MonadIO m, MonadMask m, Semigroup q, HasCallStack, HasVersion)
    => logger
    -> ChainId
    -> SQLiteEnv
    -> Parent RankedBlockHash
    -> NonEmpty (RunnableBlock m q)
    -> m q
restoreAndSave logger cid sql parent blocks = do
    -- TODO PP: check first if we're rewinding past "final" point? same with rewindTo above.
    startTxId <- liftIO $ Backend.Utils.rewindDbTo cid sql parent
    let startBlockHeight = childBlockHeight cid parent
    foldState1 (fmap executeBlock blocks) (T2 startBlockHeight startTxId)
    where

    executeBlock :: RunnableBlock m q -> T2 BlockHeight Pact.TxId -> m (q, T2 BlockHeight Pact.TxId)
    executeBlock blockAction (T2 currentBlockHeight startTxId) = do
        case blockAction of
            Pact4RunnableBlock _ block | not (pact5 cid currentBlockHeight) -> do
                let pact4TxId = Pact4.TxId (coerce startTxId)
                let blockHandlerEnv = Pact4.mkBlockHandlerEnv cid currentBlockHeight sql logger
                newBlockDbEnv <- liftIO $ newMVar $ Pact4.BlockDbEnv
                    blockHandlerEnv
                    -- FIXME not sharing the cache
                    (Pact4.initBlockState defaultModuleCacheLimit pact4TxId)
                let pactDb = Pact4.chainwebPactDb

                let pact4DbEnv = Pact4.CurrentBlockDbEnv
                        { _cpPactDbEnv = Pact4.PactDbEnv pactDb newBlockDbEnv
                        , _cpRegisterProcessedTx = \hash ->
                            Pact4.runBlockDbEnv newBlockDbEnv (Pact4.indexPactTransaction $ BS.fromShort $ Pact4.unHash $ Pact4.unRequestKey hash)
                        , _cpLookupProcessedTx = \hs -> do
                            res <- Backend.Utils.doLookupSuccessful sql currentBlockHeight (coerce hs)
                            return
                                $ HashMap.mapKeys coerce
                                $ HashMap.map
                                    (\(T3 height _payloadhash bhash) -> T2 height bhash)
                                    res
                        , _cpHeaderOracle = Pact4.headerOracleForBlock blockHandlerEnv
                        }
                !(!m, !blockInfo) <- block pact4DbEnv
                -- let outputBlockHandle =
                finalBlockState <- liftIO $
                    Pact4._benvBlockState <$> readMVar newBlockDbEnv
                -- TODO Robert: use payload hash here too
                liftIO $ Pact4.commitBlockStateToDatabase sql (fst blockInfo) (snd blockInfo) currentBlockHeight finalBlockState
                let currentRbh = RankedBlockHash currentBlockHeight (fst blockInfo)
                return
                    (m
                    , T2
                        (childBlockHeight cid (Parent currentRbh))
                        (Pact.TxId $ coerce $ Pact4._bsTxId finalBlockState))
            Pact5RunnableBlock ctx block | pact5 cid currentBlockHeight -> do
                let
                    minOracleHistory = (minimumBlockHeaderHistory (ctx ^. bctxParentForkNumber)
                                                                  currentBlockHeight)
                    blockEnv = ChainwebPactDb.BlockHandlerEnv
                        { ChainwebPactDb._blockHandlerDb = sql
                        , ChainwebPactDb._blockHandlerLogger = logger
                        , ChainwebPactDb._blockHandlerBlockHeight = currentBlockHeight
                        , ChainwebPactDb._blockHandlerChainId = cid
                        , ChainwebPactDb._blockHandlerMode = Pact.Transactional
                        , ChainwebPactDb._blockHandlerUpperBoundTxId = startTxId
                        , ChainwebPactDb._blockHandlerAtTip = True
                        }
                    pactDb = ChainwebPactDb.chainwebPactBlockDb blockEnv minOracleHistory

                -- run the block
                !((!m', !blockInfo), !blockHandle) <-
                    runStateT (block pactDb) (emptyBlockHandle startTxId)
                -- TODO PP: also check the child matches the parent height
                -- case maybeParent of
                --   Nothing
                --     | genesisHeight res.cpCwVersion res.cpChainId /= succ (_rankedBlockHashHeight nextBlock) -> error
                --       "doRestoreAndSave: block with no parent, genesis block, should have genesis height but doesn't,"
                --   Just (Parent ph)
                --     | bh /= _rankedBlockHashHeight nextBlock -> error $
                --       "doRestoreAndSave: non-genesis block should be one higher than its parent. parent at "
                --         <> sshow (_rankedBlockHashHeight ph) <> ", child height " <> sshow (_rankedBlockHashHeight nextBlock)
                --   _ -> return ()
                liftIO $ ChainwebPactDb.commitBlockStateToDatabase
                    sql
                    (Ranked currentBlockHeight blockInfo)
                    blockHandle
                    ctx
                let currentRbh = RankedBlockHash currentBlockHeight (fst blockInfo)
                return
                    (m'
                    , T2
                        (childBlockHeight cid (Parent currentRbh))
                        (_blockHandleTxId blockHandle))

            _ -> do
                error $ "pact 4/5 mismatch: " <> sshow (cid, currentBlockHeight)

getEarliestBlock :: SQLiteEnv -> IO (Maybe RankedBlockHash)
getEarliestBlock sql = do
    ChainwebPactDb.throwOnDbError $ ChainwebPactDb.getEarliestBlock sql

getLatestBlock :: SQLiteEnv -> IO (Maybe SyncState)
getLatestBlock sql = do
    ChainwebPactDb.throwOnDbError $ Backend.Utils.getLatestBlock sql

getConsensusState :: SQLiteEnv -> IO ConsensusState
getConsensusState sql = do
    ChainwebPactDb.throwOnDbError $ Backend.Utils.getConsensusState sql

setConsensusState :: SQLiteEnv -> ConsensusState -> IO ()
setConsensusState sql cs = do
    ChainwebPactDb.throwOnDbError $ Backend.Utils.setConsensusState sql cs

lookupBlockWithHeight :: HasCallStack => SQLiteEnv -> BlockHeight -> IO (Maybe (Ranked BlockHash))
lookupBlockWithHeight sql bh = do
    ChainwebPactDb.throwOnDbError $ Backend.Utils.lookupBlockWithHeight sql bh

latestBlocEnv :: HasCallStack => SQLiteEnv -> IO (Maybe (Ranked BlockHash, Parent ForkNumber, Parent BlockCreationTime))
latestBlocEnv sql = do
    ChainwebPactDb.throwOnDbError $ Backend.Utils.latestBlocEnv sql

lookupBlocEnvkWithHeight :: HasCallStack => SQLiteEnv -> BlockHeight -> IO (Maybe (Ranked BlockHash, Parent ForkNumber, Parent BlockCreationTime))
lookupBlocEnvkWithHeight sql bh = do
    ChainwebPactDb.throwOnDbError $ Backend.Utils.lookupBlocEnvkWithHeight sql bh

lookupBlockHash :: HasCallStack => SQLiteEnv -> BlockHash -> IO (Maybe BlockHeight)
lookupBlockHash sql pbh = do
    ChainwebPactDb.throwOnDbError $ Backend.Utils.lookupBlockHash sql pbh

getPayloadsAfter :: SQLiteEnv -> Parent BlockHeight -> IO [Ranked BlockPayloadHash]
getPayloadsAfter sql b = do
    ChainwebPactDb.throwOnDbError $ Backend.Utils.getPayloadsAfter sql b

-- -------------------------------------------------------------------------- --
-- Utils

-- heightProgress :: BlockHeight -> IORef BlockHeight -> (Text -> IO ()) -> IO ()
-- heightProgress initialHeight ref logFun = forever $ do
--     threadDelay (20 * 1_000_000)
--     h <- readIORef ref
--     logFun
--         $ "processed blocks: " <> sshow (h - initialHeight)
--         <> ", current height: " <> sshow h
