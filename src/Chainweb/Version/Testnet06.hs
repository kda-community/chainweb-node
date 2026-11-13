{-# language ExplicitNamespaces #-}
{-# language LambdaCase #-}
{-# language NumericUnderscores #-}
{-# language OverloadedStrings #-}
{-# language PatternSynonyms #-}
{-# language QuasiQuotes #-}
{-# language ViewPatterns #-}

module Chainweb.Version.Testnet06(testnet06, data Testnet06) where

import qualified Data.HashMap.Strict as HM
import Control.Lens
import qualified Data.Set as Set

import Chainweb.BlockCreationTime
import Chainweb.BlockHeight
import Chainweb.ChainId
import Chainweb.Difficulty
import Chainweb.Graph
import Chainweb.Pact.InitialGasModel
import Chainweb.Pact.Payload (_payloadWithOutputsPayloadHash)
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Utils.Rule
import Chainweb.Version
import P2P.BootstrapNodes

import Pact.Core.Names (VerifierName(..))

import qualified Chainweb.Pact.Transactions.OtherTransactions as CoinV2
import qualified Chainweb.Pact.Transactions.CoinV3Transactions as CoinV3
import qualified Chainweb.Pact.Transactions.CoinV4Transactions as CoinV4
import qualified Chainweb.Pact.Transactions.CoinV5Transactions as CoinV5
import qualified Chainweb.Pact.Transactions.CoinV6Transactions as CoinV6

import qualified Chainweb.BlockHeader.Genesis.Testnet060Payload as TST0
import qualified Chainweb.BlockHeader.Genesis.Testnet061to19Payload as TSTN

pattern Testnet06 :: ChainwebVersion
pattern Testnet06 <- ((== testnet06) -> True) where
    Testnet06 = testnet06

testnet06 :: ChainwebVersion
testnet06 = withVersion testnet06 $ ChainwebVersion
    { _versionCode = ChainwebVersionCode 0x00000008
    , _versionName = ChainwebVersionName "testnet06"

    , _versionForks = tabulateHashMap $ \case
        SlowEpoch -> onAllChains $ ForkAtBlockHeight $ BlockHeight 0
        Vuln797Fix -> onAllChains $ ForkAtBlockHeight $ BlockHeight 0
        PactBackCompat_v16 -> onAllChains $ ForkAtBlockHeight $ BlockHeight 0
        OldTargetGuard -> onAllChains $ ForkAtBlockHeight $ BlockHeight 0
        SkipFeatureFlagValidation -> onAllChains $ ForkAtBlockHeight $ BlockHeight 0
        SkipTxTimingValidation -> onAllChains $ ForkAtBlockHeight $ BlockHeight 2
        ModuleNameFix -> onAllChains $ ForkAtBlockHeight $ BlockHeight 2
        ModuleNameFix2 -> onAllChains $ ForkAtBlockHeight $ BlockHeight 2
        CoinV2 -> onChains $ [(unsafeChainId 0, ForkAtBlockHeight $ BlockHeight 3)] <> [(unsafeChainId i, ForkAtBlockHeight $ BlockHeight 4) | i <- [1..19]]
        OldDAGuard -> onAllChains $ ForkAtBlockHeight $ BlockHeight 13
        PactEvents -> onAllChains $ ForkAtBlockHeight $ BlockHeight 40
        SPVBridge -> onAllChains $ ForkAtBlockHeight $ BlockHeight 50
        Pact4Coin3 -> onAllChains $ ForkAtBlockHeight $ BlockHeight 80
        Pact42 -> onAllChains $ ForkAtBlockHeight $ BlockHeight 90
        EnforceKeysetFormats -> onAllChains $ ForkAtBlockHeight $ BlockHeight 100
        CheckTxHash -> onAllChains $ ForkAtBlockHeight $ BlockHeight 110
        Chainweb213Pact -> onAllChains $ ForkAtBlockHeight $ BlockHeight 95
        Chainweb214Pact -> onAllChains $ ForkAtBlockHeight $ BlockHeight 115
        Chainweb215Pact -> onAllChains $ ForkAtBlockHeight $ BlockHeight 165
        Pact44NewTrans -> onAllChains $ ForkAtBlockHeight $ BlockHeight 185
        Chainweb216Pact -> onAllChains $ ForkAtBlockHeight $ BlockHeight 215
        Chainweb217Pact -> onAllChains $ ForkAtBlockHeight $ BlockHeight 470
        Chainweb218Pact -> onAllChains $ ForkAtBlockHeight $ BlockHeight 500
        Chainweb219Pact -> onAllChains $ ForkAtBlockHeight $ BlockHeight 550
        Chainweb220Pact -> onAllChains $ ForkAtBlockHeight $ BlockHeight 560
        Chainweb221Pact -> onAllChains $ ForkAtBlockHeight $ BlockHeight 580
        Chainweb222Pact -> onAllChains $ ForkAtBlockHeight $ BlockHeight 590
        Chainweb223Pact -> onAllChains $ ForkAtBlockHeight $ BlockHeight 600
        Chainweb224Pact -> onAllChains $ ForkAtBlockHeight $ BlockHeight 610
        Chainweb225Pact -> onAllChains $ ForkAtBlockHeight $ BlockHeight 620
        Pact5Fork -> onAllChains $ ForkAtBlockHeight $ BlockHeight 640
        Chainweb228Pact -> onAllChains $ ForkAtBlockHeight $ BlockHeight 650
        Chainweb230Pact -> onAllChains $ ForkAtBlockHeight $ BlockHeight 680
        Chainweb231Pact -> onAllChains $ ForkAtBlockHeight $ BlockHeight 690
        Chainweb31 -> onAllChains $ ForkAtBlockHeight $ BlockHeight 700
        Chainweb32 -> onAllChains (ForkAtForkNumber 1)
        MigratePlatformShare -> onAllChains ForkNever
        HashedAdjacentRecord -> onAllChains ForkNever

    , _versionUpgrades = foldr (chainZip HM.union) (onAllChains mempty)
        [ indexByForkHeights
            [ (CoinV2, onAllChains (Pact4Upgrade CoinV2.transactions False))
            , (Pact4Coin3, onAllChains (Pact4Upgrade CoinV3.transactions True))
            , (Chainweb214Pact, onAllChains (Pact4Upgrade CoinV4.transactions True))
            , (Chainweb215Pact, onAllChains (Pact4Upgrade CoinV5.transactions True))
            , (Chainweb223Pact, onAllChains (Pact4Upgrade CoinV6.transactions False))
            ]
        ]

    , _versionGraphs = Bottom (minBound, twentyChainGraph)
    , _versionBlockDelay = BlockDelay 30_000_000
    , _versionWindow = WindowWidth 120
    , _versionHeaderBaseSizeBytes = 318 - 110
    , _versionBootstraps = domainAddr2PeerInfo testnet06BootstrapHosts
    , _versionGenesis = VersionGenesis
        { _genesisBlockTarget = onChains $ concat
            [ [(unsafeChainId i, HashTarget $ maxBound `div` 100_000) | i <- [0..19]]
            ]
        , _genesisTime = onAllChains $ BlockCreationTime [timeMicrosQQ| 2026-02-16T08:15:00.000000 |]
        , _genesisBlockPayload = onChains $ concat
            [ [(unsafeChainId 0, _payloadWithOutputsPayloadHash TST0.payloadBlock)]
            , [(unsafeChainId i, _payloadWithOutputsPayloadHash TSTN.payloadBlock) | i <- [1..19]]
            ]
        }
    , _versionInitialGasModel = onAllChains $
        (testnet06 ^?! versionForks . at Chainweb32 . _Just . atChain (unsafeChainId 0), post32GasModel) `Above`
        Bottom (minBound, post31GasModel)
    , _versionMaxBlockGasLimit = Bottom (minBound, Just 180_000)
    , _versionSpvProofRootValidWindow = Bottom (minBound, Nothing)
    , _versionCheats = VersionCheats
        { _disablePow = False
        , _fakeFirstEpochStart = False
        , _disablePact = False
        }
    , _versionDefaults = VersionDefaults
        { _disablePeerValidation = False
        , _disableMempoolSync = False
        }
    , _versionVerifierPluginNames = onAllChains $
        (ForkAtBlockHeight $ BlockHeight 600, Set.fromList $ map VerifierName ["hyperlane_v3_message"]) `Above`
        Bottom (minBound, mempty)
    , _versionQuirks = noQuirks
    , _versionForkNumber = 1
    , _versionForkVoteCastingLength = 120 * 119 -- 5 days
    , _versionPayloadProviderTypes = onAllChains PactProvider
    }
