{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Chainweb.BlockHeaderDBBench
-- Copyright: Copyright © 2025 Kadena LLC.
-- License: MIT
--
-- Benchmarks for 'BlockHeaderDb' queries, in particular the 'TreeDb'
-- branch traversal code paths that back header synchronization.
--
module Chainweb.BlockHeaderDBBench (bench) where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (InternalState, ResourceT)
import Control.Monad.Trans.Resource qualified as ResourceT

import Criterion.Main qualified as C

import Data.HashSet qualified as HS

import Numeric.Natural

import Streaming.Prelude qualified as S

-- internal modules

import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB
import Chainweb.Test.Utils
import Chainweb.TreeDB
import Chainweb.Utils
import Chainweb.Utils.Bench
import Chainweb.Version

-- -------------------------------------------------------------------------- --
-- Fixture

-- | A linear trunk of this many blocks on top of genesis.
trunkLength :: Natural
trunkLength = 1_000

-- | The fork branches off from the trunk at this height.
forkStartHeight :: Natural
forkStartHeight = 950

-- | The fork extends this many blocks beyond the fork point, so the fork tip
-- has the same height as the trunk tip.
forkLength :: Natural
forkLength = 50

data BenchDb = BenchDb
    { _bdbDb :: !BlockHeaderDb
    , _bdbTrunkTip :: !BlockHeader
    , _bdbForkPoint :: !BlockHeader
    , _bdbForkTip :: !BlockHeader
    }

-- | Keep a 'ResourceT' allocation alive until Criterion invokes the matching
-- cleanup action.
acquire :: ResourceT IO a -> IO (NoopNFData (a, InternalState))
acquire action = mask $ \restore -> do
    state <- ResourceT.createInternalState
    result <- restore (ResourceT.runInternalState action state)
        `onException` ResourceT.closeInternalState state
    return $ NoopNFData (result, state)

release :: NoopNFData (a, InternalState) -> IO ()
release (NoopNFData (_, state)) = ResourceT.closeInternalState state

benchDbResource :: HasVersion => ResourceT IO BenchDb
benchDbResource = do
    rocksDb <- withRocksResource
    db <- withTestBlockHeaderDb rocksDb genesis
    trunk <- liftIO $ insertN_ (Nonce 0) trunkLength genesis db
    let trunkTip = last trunk
        forkPoint = trunk !! (int forkStartHeight - 1)
    fork <- liftIO $ insertN_ (Nonce 1) forkLength forkPoint db
    return $ BenchDb db trunkTip forkPoint (last fork)
  where
    genesis = genesisBlockHeader toyChainId

-- -------------------------------------------------------------------------- --
-- Benchmarks

bench :: C.Benchmark
bench = withVersion toyVersion
    $ C.envWithCleanup (acquire benchDbResource) release
    $ \ ~(NoopNFData (bdb, _)) -> benchmarks bdb

benchmarks :: HasVersion => BenchDb -> C.Benchmark
benchmarks ~(BenchDb db trunkTip forkPoint forkTip) = C.bgroup "block-header-db"
    [ C.bgroup "getBranch"
        [ C.bench "ancestors/trunk-tip (1000 blocks)"
            $ C.nfIO
            $ S.length_ (ancestors db (key trunkTip))
        , C.bench "branchEntries/page (limit 250, maxRank=tip)"
            $ C.nfIO
            $ branchEntries db Nothing (Just 250) Nothing (Just tipRank)
                mempty (upper trunkTip)
            $ S.length_ . void
        , C.bench "branchEntries/page (limit 250, maxRank=tip, lower=fork-point)"
            $ C.nfIO
            $ branchEntries db Nothing (Just 250) Nothing (Just tipRank)
                (lower forkPoint) (upper trunkTip)
            $ S.length_ . void
        , C.bench "branchEntries/page (limit 250, maxRank=tip-500)"
            $ C.nfIO
            $ branchEntries db Nothing (Just 250) Nothing (Just midRank)
                mempty (upper trunkTip)
            $ S.length_ . void
        ]
    , C.bgroup "seekAncestor"
        [ C.bench "unique rank (tip - 500)"
            $ C.nfIO
            $ seekAncestor db trunkTip (rank trunkTip - 500)
        , C.bench "fork region (tip - 25)"
            $ C.nfIO
            $ seekAncestor db trunkTip (rank trunkTip - 25)
        ]
    , C.bgroup "fork"
        [ C.bench "forkEntry (50 block fork)"
            $ C.nfIO
            $ forkEntry db trunkTip forkTip
        , C.bench "branchDiff (50 block fork)"
            $ C.nfIO
            $ S.length_ (branchDiff db trunkTip forkTip)
        , C.bench "getBranchIncreasing (last 100 blocks)"
            $ C.nfIO
            $ getBranchIncreasing db trunkTip (rank trunkTip - 100) S.length_
        ]
    ]
  where
    tipRank = int (rank trunkTip)
    midRank = int (rank trunkTip - 500)
    upper = HS.singleton . UpperBound . key
    lower = HS.singleton . LowerBound . key
