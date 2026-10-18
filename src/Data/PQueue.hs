{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}


-- |
-- Module: Data.PQueue
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- A thread safe priority queue.
--
module Data.PQueue
( PQueue
, newEmptyPQueue
, pQueueInsert
, pQueueRemove
, pQueueIsEmpty
, pQueueSize
, pQueueEnd
, pQueueEndNice
) where

import Control.Concurrent.STM
import Control.Concurrent.Async (AsyncCancelled(..))
import Control.Monad
import Control.Exception

import Data.Ord
import qualified Data.Map as M
import qualified Data.Set as S

import Numeric.Natural

-- -------------------------------------------------------------------------- --
-- PQueue

-- | A thread safe priority queue.
--
-- The queue is fair for users of the queue. It does not guarantee progress for
-- items in the queue. An item of low priority my starve in the queue if higher
-- priority items are added at a rate at least as high as items are removed.
--

data PQueueElement a = PQueueEOF | PQueueData a

data PQueue a =
    forall p k. (Ord p, Bounded p, Ord k, Bounded k) =>
    PQueue (TVar (M.Map (Down p, k) (PQueueElement a))) (TVar (S.Set k)) (a -> p) (a -> k) (Maybe Natural)

newEmptyPQueue :: (Ord p, Bounded p, Ord k, Bounded k)  => (a -> p) -> (a -> k) -> Maybe Natural -> IO (PQueue a)
newEmptyPQueue getPrio getKey maybeMaxLen = PQueue
    <$> newTVarIO mempty
    <*> newTVarIO mempty
    <*> pure getPrio
    <*> pure getKey
    <*> pure maybeMaxLen

pQueueInsert :: PQueue a -> a -> IO ()
pQueueInsert (PQueue mv sv getPrio getKey maybeMaxLen) a =
    atomically $ do
        s <- readTVar sv
        m <- readTVar mv
        let k = getKey a
        if S.member k s
        then return ()
        else do
            let s' = S.insert k s
            let m' = M.insert (Down $ getPrio a, k) (PQueueData a) m
            let fixup (maxlen :: Natural) = if M.size m' > fromIntegral (2 * maxlen)
                    then let (keep, dontkeep) = M.splitAt (fromIntegral maxlen) m'
                    in (foldl' (flip (S.delete . snd)) s' (M.keys dontkeep), keep)
                    else (s', m')
            let (s'', m'') = maybe (s', m') fixup maybeMaxLen
            writeTVar mv $! m''
            writeTVar sv $! s''

pQueueIsEmpty :: PQueue a -> IO Bool
pQueueIsEmpty (PQueue mv _ _ _ _) = M.null <$!> readTVarIO mv

pQueueSize :: PQueue a -> IO Natural
pQueueSize (PQueue mv _ _ _ _) = fromIntegral . M.size <$!> readTVarIO mv

-- End the queue by pushing a top priority EOF
pQueueEnd :: PQueue a -> IO ()
pQueueEnd (PQueue mv _ _ _ _) = atomically $ modifyTVar mv $ M.insert (Down maxBound, minBound) PQueueEOF

-- End the queue by pushing a Lowest priority EOF
pQueueEndNice :: PQueue a -> IO ()
pQueueEndNice (PQueue mv _ _ _ _) = atomically $ modifyTVar mv $ M.insert (Down minBound, maxBound) PQueueEOF

-- | If the queue is empty it blocks and races for new items
--
pQueueRemove :: PQueue a -> IO a
pQueueRemove (PQueue mv sv _getPrio _getKey _) =
  atomically run >>= \case
    PQueueEOF -> throwIO AsyncCancelled
    PQueueData a -> return a
  where
    run = do
        m <- readTVar mv
        case M.minViewWithKey m of
            Nothing -> retry
            Just (((_, k), a), m') -> do
                writeTVar mv $! m'
                modifyTVar' sv (S.delete k)
                return a
