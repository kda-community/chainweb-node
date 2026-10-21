{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Chainweb.CutDBBench (bench) where

import Chainweb.BlockHeader
import Chainweb.BlockHeight (CutHeight)
import Chainweb.Chainweb.Configuration
import Chainweb.Cut
import Chainweb.Cut.CutHashes
import Chainweb.CutDB
import Chainweb.CutDB.RestAPI.Client
import Chainweb.Graph
import Chainweb.HostAddress
import Chainweb.Logger
import Chainweb.Pact.Payload
import Chainweb.Pact.Utils
import Chainweb.PayloadProvider
import Chainweb.RestAPI
import Chainweb.Sync.WebBlockHeaderStore
import Chainweb.Test.Cut
import Chainweb.Test.TestVersions
import Chainweb.Test.Utils
import Chainweb.Time qualified as Time
import Chainweb.Utils
import Chainweb.Utils.Bench
import Chainweb.Version
import Chainweb.WebBlockHeaderDB

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM qualified as STM
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource (InternalState, ResourceT)
import Control.Monad.Trans.Resource qualified as ResourceT

import Criterion.Main qualified as C
import Data.Foldable (toList)
import Data.HashMap.Strict qualified as HM
import Network.HTTP.Client qualified as HTTP
import Network.X509.SelfSigned
import P2P.Peer
import Servant.Client
import System.Timeout (timeout)

-- | The node that has already mined one complete catch-up step.
data Source = Source
    { sourceClientEnv :: !ClientEnv
    , sourcePeerInfo :: !PeerInfo
    }

newtype Target = Target (CutDb GenericLogger)

benchmarkVersion :: ChainwebVersion
benchmarkVersion = barebonesTestVersion twentyChainGraph

-- | Keep a 'ResourceT' allocation alive until Criterion invokes the matching
-- cleanup action.
acquire
    :: ResourceT IO a
    -> IO (NoopNFData (a, InternalState))
acquire action = mask $ \restore -> do
    state <- ResourceT.createInternalState
    result <- restore (ResourceT.runInternalState action state)
        `onException` ResourceT.closeInternalState state
    return $ NoopNFData (result, state)

release :: NoopNFData (a, InternalState) -> IO ()
release (NoopNFData (_, state)) = ResourceT.closeInternalState state

-- | Add the given number of microseconds of synthetic latency to every
-- HTTP request issued through the manager, to emulate internetwork
-- conditions.
addLatency :: Int -> HTTP.ManagerSettings -> HTTP.ManagerSettings
addLatency us s = s
    { HTTP.managerModifyRequest =
        \req -> threadDelay us >> HTTP.managerModifyRequest s req
    }

newCutDb :: HasVersion => Int -> ResourceT IO (CutDb GenericLogger)
newCutDb latencyUs = do
    rocksDb <- withRocksResource
    webDb <- liftIO $ initWebBlockHeaderDb rocksDb
    managerSettings <- liftIO
        $ addLatency latencyUs <$> certificateCacheManagerSettings TlsInsecure
    httpManager <- liftIO $ HTTP.newManager managerSettings
    headerStore <- liftIO $ newWebBlockHeaderStore httpManager webDb noopLogger
    Right cutDb <- withCutDb
        (defaultCutDbParams 4_125_000)
        noopLogger
        headerStore
        (onAllChains DisabledPayloadProvider)
        (cutHashesTable rocksDb)
    return cutDb

-- | Upper bound (in microseconds) on how long to wait for the cut
-- database to accept a cut before declaring the catch-up failed. Must
-- comfortably exceed the cut database's fetch timeout plus the synthetic
-- per-request latency, or slow-but-successful catch-ups would be
-- misreported as failures.
awaitCutTimeoutUs :: Int
awaitCutTimeoutUs = 120_000_000

-- | Wait for the cut database to accept a cut of at least the given
-- height. Cuts whose prerequisite headers cannot be fetched within the
-- cut database's fetch timeout are dropped, so acceptance is not
-- guaranteed and an unconditional 'STM.check' would block forever.
awaitCutHeight :: CutDb GenericLogger -> CutHeight -> IO ()
awaitCutHeight cutDb height = do
    result <- timeout awaitCutTimeoutUs $ STM.atomically $ do
        acceptedCut <- _cutStm cutDb
        STM.check $ _cutHeight acceptedCut >= height
    case result of
        Nothing -> throwIO $ userError
            $ "cut database did not accept a cut of height "
            <> show height
            <> " within "
            <> show awaitCutTimeoutUs
            <> "us"
        Just _ -> pure ()

-- | The largest catch-up step the cut database accepts from a single
-- submission: bounded by 'farAheadThreshold' relative to the current
-- cut (cf. 'syncSession' in "Chainweb.CutDB.Sync", which additionally
-- caps this at 'catchupStepSize' = 500).
stepSize :: HasVersion => CutHeight
stepSize = int farAheadThreshold * int (length (toList chainIds)) - 1

mineCatchupStep :: HasVersion => CutDb GenericLogger -> IO ()
mineCatchupStep cutDb = do
    start <- _cut cutDb
    void $ foldM mineOne start miningPlan
  where
    webDb = view cutDbWebBlockHeaderDb cutDb
    payloadHash = _payloadWithOutputsPayloadHash emptyPayload

    mineOne !currentCut (nonce, cid) = do
        let creationTime = offsetBlockTime Time.second currentCut cid
        Right (T2 header nextCut) <- testMineWithPayloadHash
            webDb
            (Nonce nonce)
            creationTime
            payloadHash
            cid
            currentCut
        addCutHashes cutDb $ (cutToCutHashes Nothing nextCut)
            { _cutHashesHeaders = HM.singleton (view blockHash header) header
            }
        awaitCutHeight cutDb $ _cutHeight nextCut
        return nextCut

    -- All chains advance together in balanced rounds, so the maximum
    -- skew between chains remains below 'farAheadThreshold'.
    miningPlan = zip [0 ..]
        $ take (int stepSize)
        $ cycle
        $ toList chainIds

sourceResource :: HasVersion => Int -> ResourceT IO Source
sourceResource latencyUs = do
    cutDb <- newCutDb latencyUs
    liftIO $ mineCatchupStep cutDb
    minedCut <- liftIO $ _cut cutDb
    liftIO $ unless (_cutHeight minedCut == stepSize)
        $ throwIO
        $ userError "mined cut does not span a complete catch-up step"

    let serverDbs :: ChainwebServerDbs GenericLogger ()
        serverDbs = emptyChainwebServerDbs
            { _chainwebServerCutDb = Just cutDb
            , _chainwebServerBlockHeaderDbs =
                view (cutDbWebBlockHeaderDb . webBlockHeaderDb) cutDb
            }
        app = chainwebApplication
            (defaultChainwebConfiguration implicitVersion)
            serverDbs

    port <- withChainwebTestServer DoNotValidateSpec True app
    managerSettings <- liftIO
        $ addLatency latencyUs <$> certificateCacheManagerSettings TlsInsecure
    httpManager <- liftIO $ HTTP.newManager managerSettings
    let clientEnv = mkClientEnv httpManager $ BaseUrl Https testHost port ""
        originPeer = PeerInfo Nothing $ HostAddress localhost
            $ fromJuste
            $ portFromInt port
    return $ Source clientEnv originPeer

targetResource :: HasVersion => Int -> ResourceT IO Target
targetResource latencyUs = Target <$> newCutDb latencyUs

-- | Iteratively pull and apply cuts, mirroring the receive loop of
-- 'syncSession'. A single submission of the full mined step would be
-- discarded as "far ahead" (cf. 'farAheadThreshold'), so the target
-- walks up to the mined height in bounded steps, exactly as a real node
-- does during catch-up.
catchUp :: HasVersion => Source -> Target -> IO ()
catchUp source (Target target) = go
  where
    go = do
        cur <- _cut target
        let curHeight = _cutHeight cur
            limit = curHeight + stepSize
        when (curHeight < stepSize) $ do
            remoteCut <- either throwIO return =<< runClientM
                (cutGetClientLimit $ int limit)
                (sourceClientEnv source)
            let remoteHeight = _cutHashesHeight remoteCut
            unless (remoteHeight > curHeight)
                $ throwIO
                $ userError "source cut made no progress during catch-up"
            addCutHashes target $ remoteCut
                { _cutOrigin = Just $ sourcePeerInfo source
                }
            awaitCutHeight target remoteHeight
            go

bench :: C.Benchmark
bench = withVersion benchmarkVersion
    $ C.bgroup "cut-db"
        [ variant "local" 0
        , variant "100ms latency" 100_000
        ]
  where
    variant :: HasVersion => String -> Int -> C.Benchmark
    variant label latencyUs = C.envWithCleanup
        (acquire $ sourceResource latencyUs)
        release
        $ \ ~(NoopNFData (source, _)) -> C.bench
            (label <> ": catch up one "
                <> show (int stepSize :: Int)
                <> "-block step from genesis")
            $ C.perRunEnvWithCleanup
                (acquire $ targetResource latencyUs)
                release
                $ \ ~(NoopNFData (target, _)) -> catchUp source target
