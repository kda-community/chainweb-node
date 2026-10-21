{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Chainweb.Scheduler
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Stability: experimental
--
-- A small scheduler for cron-ish tasks that runs jobs on a bounded thread
-- pool.
--
-- Each task is a recurring action with a name and an interval. A task
-- occupies a worker thread only for a single run of its action: after each
-- run the task is requeued for its next interval. Synchronous exceptions in
-- the action are logged and the task keeps its schedule (mirroring
-- 'Chainweb.Utils.runForever'); asynchronous exceptions propagate and stop
-- the worker.
--
-- Tasks are kept in a time-priority queue: workers always pick the task that
-- is due next, and a worker that is waiting for a future task wakes up
-- immediately when an earlier task is submitted.
--
module Chainweb.Scheduler
(
-- * Scheduler
  Scheduler
, withScheduler
, schedule
, scheduleEvery

-- * Task
, Task(..)
, seconds
, minutes
) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (ResourceT, allocate)

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Time.Clock.POSIX (getPOSIXTime)

import qualified Data.Text as T

import System.LogLevel

import Chainweb.Utils (catchAllSynchronous, sshow)

-- -------------------------------------------------------------------------- --
-- Task

-- | A recurring task: a name, an interval in microseconds between runs, and
-- the action to run. The action runs once per interval; after each run the
-- task is requeued, so it never occupies a worker thread for more than one
-- run.
--
data Task = Task
    { _taskName :: !T.Text
    , _taskInterval :: !Int
        -- ^ interval between runs in microseconds
    , _taskAction :: IO ()
    }

-- | One second in microseconds.
seconds :: Int -> Int
seconds = (* 1000000)

-- | One minute in microseconds.
minutes :: Int -> Int
minutes = (* 60000000)

-- -------------------------------------------------------------------------- --
-- Scheduler

-- | Time-priority queue of scheduled tasks, keyed by due time.
--
newtype TaskQueue = TaskQueue (TVar (Map Integer [Task]))

newTaskQueue :: IO TaskQueue
newTaskQueue = TaskQueue <$> newTVarIO M.empty

-- | Return the due time of the earliest task, blocking if the queue is
-- empty.
--
peekDue :: TaskQueue -> STM Integer
peekDue (TaskQueue v) = do
    m <- readTVar v
    case M.lookupMin m of
        Nothing -> retry
        Just (due, _) -> return due

-- | Pop the earliest task together with its due time. Only safe to call when
-- the earliest task is due.
--
popTask :: TaskQueue -> STM (Integer, Task)
popTask (TaskQueue v) = do
    m <- readTVar v
    case M.lookupMin m of
        Just (due, t : ts) -> do
            let m' = if null ts then M.delete due m else M.insert due ts m
            writeTVar v $! m'
            return (due, t)
        _ -> retry

enqueueTask :: TaskQueue -> Integer -> Task -> STM ()
enqueueTask (TaskQueue v) due task =
    modifyTVar' v $ M.insertWith (flip (++)) due [task]

-- | A scheduler that runs tasks on a bounded thread pool.
--
data Scheduler = Scheduler
    { _schedulerLog :: LogLevel -> T.Text -> IO ()
    , _schedulerQueue :: TaskQueue
    }

-- | Create a scheduler with the given logging function and number of worker
-- threads. The worker threads are terminated when the surrounding
-- 'ResourceT' region exits.
--
withScheduler
    :: (LogLevel -> T.Text -> IO ())
        -- ^ logging function
    -> Int
        -- ^ number of worker threads
    -> ResourceT IO Scheduler
withScheduler logfun n = do
    q <- liftIO newTaskQueue
    let sched = Scheduler logfun q
    _ <- allocate
        (async (mapConcurrently_ (const $ worker sched) [1..max 1 n]))
        cancel
    return sched
  where
    worker sched = forever $ do
        waitForDue sched
        (due, task) <- atomically $ popTask (_schedulerQueue sched)
        runOnce sched task
        reschedule sched due task

    -- Block until the earliest task in the queue is due, waking early if an
    -- earlier task is submitted.
    waitForDue sched = do
        due <- atomically $ peekDue (_schedulerQueue sched)
        now <- nowUs
        if due <= now
            then return ()
            else do
                timer <- registerDelay (fromIntegral (due - now))
                atomically $
                    (check =<< readTVar timer) `orElse` do
                        due' <- peekDue (_schedulerQueue sched)
                        check (due' < due)

    -- Run a single iteration, recovering from synchronous exceptions like
    -- 'Chainweb.Utils.runForever' does.
    runOnce sched task =
        _taskAction task
            `catchAllSynchronous` \e ->
                _schedulerLog sched Error $
                    _taskName task <> " failed: " <> sshow e <> ". Continuing ..."

    -- Requeue the task one interval after its previous due time, so the
    -- schedule doesn't drift when runs finish late.
    reschedule sched due task = do
        let due' = due + fromIntegral (_taskInterval task)
        atomically $ enqueueTask (_schedulerQueue sched) due' task

nowUs :: IO Integer
nowUs = fmap (floor . (* 1000000)) getPOSIXTime

-- | Submit a task to the scheduler. The first run happens one interval from
-- now.
--
schedule :: Scheduler -> Task -> IO ()
schedule sched task = do
    due <- nowUs
    let due' = due + fromIntegral (_taskInterval task)
    atomically $ enqueueTask (_schedulerQueue sched) due' task

-- | Submit a recurring task that runs at the given interval (in
-- microseconds).
--
scheduleEvery :: Scheduler -> T.Text -> Int -> IO () -> IO ()
scheduleEvery sched name interval action =
    schedule sched (Task name interval action)
