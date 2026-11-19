{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Chainweb.Test.BlockHeaderDBBranch
-- Copyright: Copyright © 2026 Kadena LLC.
-- License: MIT
-- Stability: experimental
--
-- Tests that the 'branchEntries' implementation of the 'BlockHeaderDb' instance
-- of 'TreeDb' agrees with the generic implementation.
--
module Chainweb.Test.BlockHeaderDBBranch (tests) where

import Control.Monad (filterM, forM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource

import Data.Foldable (toList, traverse_)
import qualified Data.HashSet as HS
import qualified Data.List as L
import Data.Semigroup (Max(..), Min(..))
import Data.Tree (Tree(..))

import Numeric.Natural (Natural)

import qualified Streaming.Prelude as S

import Test.QuickCheck
    ( Arbitrary(..)
    , Gen
    , Property
    , arbitrary
    , choose
    , elements
    , forAll
    , frequency
    , ioProperty
    , resize
    , shrinkList
    , vectorOf
    )
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty)

-- internal modules

import Chainweb.BlockHash (BlockHash, nullBlockHash)
import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB
import Chainweb.BlockHeaderDB.Internal (unsafeInsertBlockHeaderDb)
import Chainweb.Test.Utils
import Chainweb.TreeDB
import Chainweb.Utils (int)
import Chainweb.Utils.Paging
import Chainweb.Version (HasVersion, withVersion)

import Chainweb.Storage.Table.RocksDB

-- | The parameters of a branch query
data Query = Query
    { _qNext :: Maybe (NextItem BlockHash)
    , _qLimit :: Maybe Limit
    , _qMinRank :: Maybe MinRank
    , _qMaxRank :: Maybe MaxRank
    , _qLower :: HS.HashSet (LowerBound BlockHash)
    , _qUpper :: HS.HashSet (UpperBound BlockHash)
    } deriving (Show)

-- | Generate a query for the headers of the given tree
genQuery :: HasVersion => Tree BlockHeader -> Gen Query
genQuery t = do
    let hs = map key (toList t)
    upper <- genSubset hs
    lower <- genSubset hs
    nxt <- frequency [(1, pure Nothing), (2, Just <$> (wrapNext =<< elements hs))]
    Query nxt
        <$> genMaybe (nat 12)
        <*> genMaybe (minRank 20)
        <*> genMaybe (maxRank 20)
        <*> pure (HS.fromList (map LowerBound lower))
        <*> pure (HS.fromList (map UpperBound upper))
  where
    nat :: Natural -> Gen Limit
    nat n = (\i -> Limit (int i)) <$> choose (0, n)
    minRank :: Natural -> Gen MinRank
    minRank n = (\i -> MinRank (Min (int i))) <$> choose (0, n)
    maxRank :: Natural -> Gen MaxRank
    maxRank n = (\i -> MaxRank (Max (int i))) <$> choose (0, n)
    wrapNext h = frequency [(1, pure (Inclusive h)), (1, pure (Exclusive h))]
    genMaybe g = frequency [(1, pure Nothing), (2, Just <$> g)]
    genSubset xs = filterM (const arbitrary) xs

-- | Compare the result of 'branchEntries' with the result of the generic
-- implementation 'chainBranchEntries' for the same query.
--
compareBranch :: HasVersion => BlockHeaderDb -> Query -> IO Bool
compareBranch db (Query k l mir mar lower upper) = do
    (xs S.:> ra) <- branchEntries db k l mir mar lower upper S.toList
    (ys S.:> rb) <- chainBranchEntries db k l mir mar lower upper S.toList
    -- Within a rank the order of the streamed entries is unspecified, so the
    -- results are compared as sorted lists.
    let norm = L.sort . map key
    return $ norm xs == norm ys && ra == rb

-- | Compare both implementations for randomly generated queries on randomly
-- generated trees.
--
prop_equiv :: HasVersion => RocksDb -> Property
prop_equiv rdb = forAll (resize 60 arbitrary) $ \(SparseTree t) ->
    forAll (genQuery t) $ \q -> ioProperty $
    runResourceT $ do
        db <- withTestBlockHeaderDb rdb (rootLabel t)
        liftIO $ traverse_ (unsafeInsertBlockHeaderDb db) (toList t)
        liftIO $ compareBranch db q

-- | A chain that consists of a single trunk and a number of forks.
data Shape = Shape
    { _shapeTrunkLen :: !Int
        -- ^ Length of the trunk
    , _shapeForks :: ![(Int, Int, Nonce)]
        -- ^ (height of the fork point, length of the fork, nonce of the fork)
    } deriving (Show)

instance Arbitrary Shape where
    arbitrary = do
        n <- choose (20, 100)
        k <- choose (0, 2)
        fs <- vectorOf k $ do
            at <- choose (0, n - 1)
            l <- choose (1, 40)
            nonce <- Nonce <$> arbitrary
            return (at, l, nonce)
        return (Shape n fs)
    shrink (Shape n fs) =
        [ Shape n' fs
        | n' <- shrink n
        , n' > 0
        ] ++
        [ Shape n fs'
        | fs' <- shrinkList shrinkTuple fs
        ]
      where
        shrinkTuple (at, l, nonce) =
            [ (at', l', nonce')
            | at' <- shrink at
            , at' >= 0
            , at' < n
            , l' <- shrink l
            , l' > 0
            , nonce' <- [nonce]
            ]

-- | A branch query that is specified in terms of indexes into the headers of a
-- 'Shape'. This allows the query to be generated before the headers of the
-- shape are known.
data QuerySpec = QuerySpec
    { _qsUpper :: [Int]
    , _qsLower :: [Int]
    , _qsCursor :: Maybe (Bool, Int)
        -- ^ (is exclusive, index)
    , _qsLimit :: Maybe Int
    , _qsMinRank :: Maybe Int
    , _qsMaxRank :: Maybe Int
    , _qsBogusUpper :: Bool
    , _qsBogusLower :: Bool
    } deriving (Show)

-- | The number of headers of a shape
shapeCount :: Shape -> Int
shapeCount (Shape n fs) = 1 + n + sum (map (\(_, l, _) -> l) fs)

genQuerySpec :: Shape -> Gen QuerySpec
genQuerySpec shape = do
    let count = shapeCount shape
    QuerySpec
        <$> genSubset [0 .. count - 1]
        <*> genSubset [0 .. count - 1]
        <*> genMaybe ((,) <$> arbitrary <*> choose (0, count - 1))
        <*> genMaybe (choose (0, count))
        <*> genMaybe (choose (0, count))
        <*> genMaybe (choose (0, count))
        <*> arbitrary
        <*> arbitrary
  where
    genMaybe g = frequency [(1, pure Nothing), (2, Just <$> g)]
    genSubset xs = filterM (const arbitrary) xs

-- | Turn a 'QuerySpec' into a 'Query' for the given headers. The headers must
-- be the headers of the shape for which the spec was generated.
resolveQuery :: HasVersion => [BlockHeader] -> QuerySpec -> Query
resolveQuery hs (QuerySpec upper lower cursor lim mir mar bogusU bogusL) = Query
    (fmap (\(ex, i) -> wrap (hs !! i) ex) cursor)
    (fmap (Limit . int) lim)
    (fmap (MinRank . Min . int) mir)
    (fmap (MaxRank . Max . int) mar)
    (addBogus bogusL LowerBound lower)
    (addBogus bogusU UpperBound upper)
  where
    wrap h ex = if ex then Exclusive (key h) else Inclusive (key h)
    addBogus False f is = HS.fromList (map (f . key . (hs !!)) is)
    addBogus True f is = HS.insert (f nullBlockHash)
        $ HS.fromList (map (f . key . (hs !!)) is)

-- | Insert the headers of the given shape into the database and return them in
-- the canonical order that 'resolveQuery' expects.
insertShape :: HasVersion => BlockHeaderDb -> BlockHeader -> Shape -> IO [BlockHeader]
insertShape db gen (Shape n fs) = do
    trunk <- insertN_ (Nonce 0) (int n) gen db
    forks <- fmap concat $ forM fs $ \(at, l, nonce) ->
        insertN_ nonce (int l) (trunk !! at) db
    return (gen : trunk ++ forks)

-- | Compare both implementations for randomly generated queries on chains that
-- consist of a long trunk and a number of forks of random length at random
-- heights.
--
prop_deep :: HasVersion => RocksDb -> Property
prop_deep rdb = forAll arbitrary $ \shape ->
    forAll (genQuerySpec shape) $ \qs -> ioProperty $
    runResourceT $ do
        (gen, db) <- withToyDB rdb toyChainId
        liftIO $ do
            hs <- insertShape db gen shape
            compareBranch db (resolveQuery hs qs)

-- | Compare both implementations for a large number of queries on a chain that
-- consists of a long trunk and a long fork.
--
deepBranchEquivalence :: HasVersion => RocksDb -> Assertion
deepBranchEquivalence rdb = runResourceT $ do
    (gen, db) <- withToyDB rdb toyChainId
    liftIO $ do
        trunk <- insertN_ (Nonce 0) 200 gen db
        let forkPoint = trunk !! 99
        fork <- insertN_ (Nonce 1) 150 forkPoint db
        let trunkTip = last trunk
            forkTip = last fork
            mid = trunk !! 150
            bogus = nullBlockHash
            both = HS.fromList [UpperBound (key trunkTip), UpperBound (key forkTip)]
            queries =
                [ Query nxt lim
                    (fmap (MinRank . Min) i)
                    (fmap (MaxRank . Max) j)
                    lower upper
                | i <- [Nothing, Just 0, Just 100]
                , j <- [Nothing, Just 50, Just 200]
                , nxt <- [Nothing, Just (Inclusive (key mid)), Just (Exclusive (key mid))]
                , lim <- [Nothing, Just 0, Just 7, Just 1000]
                , lower <-
                    [ HS.empty
                    , HS.singleton (LowerBound (key forkPoint))
                    , HS.singleton (LowerBound bogus)
                    ]
                , upper <-
                    [ HS.singleton (UpperBound (key trunkTip))
                    , HS.singleton (UpperBound (key forkTip))
                    , both
                    ]
                ]
        results <- forM queries (compareBranch db)
        assertBool "branchEntries doesn't match the generic implementation" (and results)

tests :: RocksDb -> TestTree
tests rdb = withVersion toyVersion $ testGroup "branchEntries"
    [ testProperty "matches the generic implementation" (prop_equiv rdb)
    , testProperty "matches the generic implementation (trunk and forks)"
        (prop_deep rdb)
    , testCase "matches the generic implementation (trunk and fork)"
        (deepBranchEquivalence rdb)
    ]
