{-# language ScopedTypeVariables #-}

-- |
-- Module: Data.Test.PQueue
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Data.Test.PQueue
(
  properties

-- * Indiviual Properties
, prop_empty
, prop_insert
, prop_insert_remove_null
, prop_insert_remove_null_concurrent
, prop_insert_remove_sort
, prop_insert_remove_concurrent
) where

import Control.Concurrent.Async
import Control.Monad.IO.Class

import Data.Foldable
import qualified Data.List as L
import Data.Maybe
import Data.Ord

import Test.QuickCheck
import Test.QuickCheck.Monadic

-- internal modules

import Data.PQueue

-- -------------------------------------------------------------------------- --
-- Properties

properties :: [(String, Property)]
properties =
    [ ("newEmptyPQueue is empty" , property $ prop_empty)
    , ("inserts result in queue of correct size" , property $ prop_insert)
    , ("equal number of inserts and remove result in empty queue" , property $ prop_insert_remove_null)
    , ("equal number of concurrent inserts and remove result in empty queue" , property $ prop_insert_remove_null_concurrent)
    , ("inserting and removeing a list sorts the list", property $ prop_insert_remove_sort)
    , ("concurrently inserting and removing a list yields the items of original list" , property $ prop_insert_remove_concurrent)
    ]

-- -------------------------------------------------------------------------- --
-- Tests

prop_empty :: Property
prop_empty = once $ monadicIO $ do
    q :: PQueue Int <- run $ newEmptyPQueue id id Nothing
    x <- run (pQueueIsEmpty q)
    assert x
    s <- run (pQueueSize q)
    assert $ s == 0

prop_insert :: [Int] -> Property
prop_insert l = monadicIO $ do
    let l' = zip [0 :: Int .. ] l
    s <- run $ do
        q <- newEmptyPQueue snd fst Nothing
        traverse_ (pQueueInsert q) l'
        pQueueSize q
    assert $ s == fromIntegral (length l)

prop_insert_remove_null :: [Int] -> Property
prop_insert_remove_null l = monadicIO $ do
    let l' = zip [0 :: Int .. ] l
    q <- run $ newEmptyPQueue snd fst Nothing
    s <- run $ do
        traverse_ (pQueueInsert q) l'
        traverse_ (const $ pQueueRemove q) l'
        pQueueSize q
    assert $ s == 0
    assert =<< run (pQueueIsEmpty q)

prop_insert_remove_null_concurrent :: [Int] -> Property
prop_insert_remove_null_concurrent l = monadicIO $ do
    let l' = zip [0 :: Int .. ] l
    q <- run $ newEmptyPQueue snd fst Nothing
    run $ concurrently_
        (traverse_ (pQueueInsert q) l')
        (traverse_ (const $ pQueueRemove q) l')
    s <- run $ pQueueSize q
    assert $ s == 0
    assert =<< run (pQueueIsEmpty q)

prop_insert_remove_sort :: [Int] -> Property
prop_insert_remove_sort l = monadicIO $ do
    let l' = zip [0 :: Int .. ] l
    q <- run $ newEmptyPQueue snd fst Nothing
    l'' <- run $ do
        traverse_ (pQueueInsert q) l'
        traverse (const $ pQueueRemove q) l'
    return $ L.sortOn (Down . snd) l'' === l''

prop_insert_remove_concurrent :: [Int] -> Property
prop_insert_remove_concurrent l = monadicIO $ do
    let l' = zip [0 :: Int .. ] l
    q <- run $ newEmptyPQueue snd fst Nothing
    commands <- pick $ shuffle
        $ (QueueInsert <$> l') ++ (QueueRemove <$ l')
    l'' <- run $ catMaybes
        <$> mapConcurrently (runQueueCommand q) commands
    return $ l' === L.sortOn fst l''

-- -------------------------------------------------------------------------- --
-- Utils

data QueueCommand a = QueueInsert a | QueueRemove
    deriving (Show)

runQueueCommand :: MonadIO m => Ord a => PQueue a -> QueueCommand a -> m (Maybe a)
runQueueCommand q (QueueInsert a) = liftIO (Nothing <$ pQueueInsert q a)
runQueueCommand q QueueRemove = liftIO (Just <$> pQueueRemove q)
