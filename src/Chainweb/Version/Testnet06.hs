{-# language LambdaCase #-}
{-# language NumericUnderscores #-}
{-# language OverloadedStrings #-}
{-# language PatternSynonyms #-}
{-# language QuasiQuotes #-}
{-# language ViewPatterns #-}

module Chainweb.Version.Testnet06(testnet06, pattern Testnet06) where

import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set

import Chainweb.BlockCreationTime
import Chainweb.BlockHeight
import Chainweb.ChainId
import Chainweb.Difficulty
import Chainweb.Graph
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Utils.Rule
import Chainweb.Version
import P2P.BootstrapNodes

import Pact.Types.Verifier

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
testnet06 = ChainwebVersion
    { _versionCode = ChainwebVersionCode 0x00000008
    , _versionName = ChainwebVersionName "testnet06"

    , _versionForks = tabulateHashMap $ \case
        SlowEpoch -> AllChains $ ForkAtBlockHeight $ BlockHeight 0
        Vuln797Fix -> AllChains $ ForkAtBlockHeight $ BlockHeight 0
        PactBackCompat_v16 -> AllChains $ ForkAtBlockHeight $ BlockHeight 0
        OldTargetGuard -> AllChains $ ForkAtBlockHeight $ BlockHeight 0
        SkipFeatureFlagValidation -> AllChains $ ForkAtBlockHeight $ BlockHeight 0
        SkipTxTimingValidation -> AllChains $ ForkAtBlockHeight $ BlockHeight 2
        ModuleNameFix -> AllChains $ ForkAtBlockHeight $ BlockHeight 2
        ModuleNameFix2 -> AllChains $ ForkAtBlockHeight $ BlockHeight 2
        CoinV2 -> onChains $ [(unsafeChainId 0, ForkAtBlockHeight $ BlockHeight 3)] <> [(unsafeChainId i, ForkAtBlockHeight $ BlockHeight 4) | i <- [1..19]]
        OldDAGuard -> AllChains $ ForkAtBlockHeight $ BlockHeight 13
        PactEvents -> AllChains $ ForkAtBlockHeight $ BlockHeight 40
        SPVBridge -> AllChains $ ForkAtBlockHeight $ BlockHeight 50
        Pact4Coin3 -> AllChains $ ForkAtBlockHeight $ BlockHeight 80
        Pact42 -> AllChains $ ForkAtBlockHeight $ BlockHeight 90
        EnforceKeysetFormats -> AllChains $ ForkAtBlockHeight $ BlockHeight 100
        CheckTxHash -> AllChains $ ForkAtBlockHeight $ BlockHeight 110
        Chainweb213Pact -> AllChains $ ForkAtBlockHeight $ BlockHeight 95
        Chainweb214Pact -> AllChains $ ForkAtBlockHeight $ BlockHeight 115
        Chainweb215Pact -> AllChains $ ForkAtBlockHeight $ BlockHeight 165
        Pact44NewTrans -> AllChains $ ForkAtBlockHeight $ BlockHeight 185
        Chainweb216Pact -> AllChains $ ForkAtBlockHeight $ BlockHeight 215
        Chainweb217Pact -> AllChains $ ForkAtBlockHeight $ BlockHeight 470
        Chainweb218Pact -> AllChains $ ForkAtBlockHeight $ BlockHeight 500
        Chainweb219Pact -> AllChains $ ForkAtBlockHeight $ BlockHeight 550
        Chainweb220Pact -> AllChains $ ForkAtBlockHeight $ BlockHeight 560
        Chainweb221Pact -> AllChains $ ForkAtBlockHeight $ BlockHeight 580
        Chainweb222Pact -> AllChains $ ForkAtBlockHeight $ BlockHeight 590
        Chainweb223Pact -> AllChains $ ForkAtBlockHeight $ BlockHeight 600
        Chainweb224Pact -> AllChains $ ForkAtBlockHeight $ BlockHeight 610
        Chainweb225Pact -> AllChains $ ForkAtBlockHeight $ BlockHeight 620
        Pact5Fork -> AllChains $ ForkAtBlockHeight $ BlockHeight 640
        Chainweb228Pact -> AllChains $ ForkAtBlockHeight $ BlockHeight 650
        Chainweb230Pact -> AllChains $ ForkAtBlockHeight $ BlockHeight 680
        Chainweb231Pact -> AllChains $ ForkAtBlockHeight $ BlockHeight 690
        Chainweb31 -> AllChains $ ForkAtBlockHeight $ BlockHeight 700
        MigratePlatformShare -> AllChains $ ForkNever

    , _versionUpgrades = foldr (chainZip HM.union) (AllChains mempty)
        [ indexByForkHeights testnet06
            [ (CoinV2, AllChains (Pact4Upgrade CoinV2.transactions False))
            , (Pact4Coin3, AllChains (Pact4Upgrade CoinV3.transactions True))
            , (Chainweb214Pact, AllChains (Pact4Upgrade CoinV4.transactions True))
            , (Chainweb215Pact, AllChains (Pact4Upgrade CoinV5.transactions True))
            , (Chainweb223Pact, AllChains (Pact4Upgrade CoinV6.transactions False))
            ]
        ]

    , _versionGraphs = Bottom (minBound, twentyChainGraph)
    , _versionBlockDelay = BlockDelay 30_000_000
    , _versionWindow = WindowWidth 120
    , _versionHeaderBaseSizeBytes = 318 - 110
    , _versionBootstraps = domainAddr2PeerInfo testnet06BootstrapHosts
    , _versionGenesis = VersionGenesis
        -- TODO Setup properly here
        { _genesisBlockTarget = onChains $ concat
            [ [(unsafeChainId i, HashTarget $ maxBound `div` 100_000) | i <- [0..19]]
            ]
        -- TODO Setup Genesis time properly
        , _genesisTime = AllChains $ BlockCreationTime [timeMicrosQQ| 2019-07-17T18:28:37.613832 |]
        , _genesisBlockPayload = onChains $ concat
            [ [(unsafeChainId 0, TST0.payloadBlock)]
            , [(unsafeChainId i, TSTN.payloadBlock) | i <- [1..19]]
            ]
        }

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
    , _versionVerifierPluginNames = AllChains $
        (600, Set.fromList $ map VerifierName ["hyperlane_v3_message"]) `Above`
        Bottom (minBound, mempty)
    , _versionQuirks = noQuirks
    , _versionForkNumber = 0
    , _versionForkVoteCastingLength = 120 * 119 -- 5 days
    }
