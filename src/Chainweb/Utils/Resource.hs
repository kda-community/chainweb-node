{-# LANGUAGE ScopedTypeVariables #-}

-- | Resource management and concurrent initialization utilities.
module Chainweb.Utils.Resource
( allocateConcurrently, withLink, withAsyncR, resourceToBracket
, concurrentlies, concurrentlies_
) where

import Control.Concurrent.Async
import Control.Monad.Catch (uninterruptibleMask_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource
import Data.Foldable (traverse_)
import Data.IORef (readIORef, writeIORef)

-- -------------------------------------------------------------------------- --
-- Resource Management

-- | Concurrently initializes resources.
--
-- This function is a bit of a hack that allows allocating several resources
-- concurrently and providing them to a single inner computation.
--
allocateConcurrently
    :: forall a t
    . Traversable t
    => t (ResourceT IO a)
    -> ResourceT IO (t a)
allocateConcurrently mkResources = do
    (_, resourcesAndInternalStates) <- allocate (
        -- threads inherit masking state, and allocate runs initializers masked.
        forConcurrently mkResources $ \mkResource -> do
            internalState <- createInternalState
            -- runInternalState doesn't actually clean up the resources.
            resource <- runInternalState mkResource internalState
            return (resource, internalState)
        )
        -- likewise finalizers are run masked.
        (mapConcurrently_ (closeInternalState . snd))
    return $ fst <$> resourcesAndInternalStates

-- | Run an async IO action, link it back to the main thread, and return
-- the async result
--
withLink :: forall a. IO a -> IO (Async a)
withLink act = do
  a <- async act
  link a
  return a

-- | Spawn a thread to do the input action and kill it on exit.
withAsyncR :: IO a -> ResourceT IO (Async a)
withAsyncR act = snd <$> allocate (async act) uninterruptibleCancel

-- | Use a ResourceT-allocated resource from @bracket@.
resourceToBracket
    :: ResourceT IO a
    -> (IO (a, InternalState)
        , (a, InternalState) -> IO ())
resourceToBracket res = do
    let prime = do
            r <- res
            uninterruptibleMask_ $ do
                s <- getInternalState
                releaseMap <- liftIO $ readIORef s
                s' <- createInternalState
                liftIO $ writeIORef s =<< readIORef s'
                liftIO $ writeIORef s' releaseMap
                return (r, s')

    (runResourceT prime, \(_, s) -> closeInternalState s)

-- | Like `sequence` for IO but concurrent
concurrentlies :: forall a. [IO a] -> IO [a]
concurrentlies = runConcurrently . traverse Concurrently

-- | Like `sequence_` for IO but concurrent
concurrentlies_ :: forall a. [IO a] -> IO ()
concurrentlies_ = runConcurrently . traverse_ Concurrently
