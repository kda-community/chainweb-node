{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- TODO Remove this when checkFork' will be used for real
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module: Chainweb.Version.Guards
-- Copyright: Copyright © 2023 Kadena LLC.
-- License: MIT
-- Maintainer: Edmund Noble <edmund@kadena.io>
-- Stability: experimental
--
-- Functions which dictate changes in block validation behavior at different
-- BlockHeights, based on chainweb versions.
--
-- Changes either activate at a certain block height and for all subsequent
-- blocks, activate for all subsequent blocks after a certain block height, or
-- activate for all previous blocks before a certain block height.
--

module Chainweb.Version.Guards
    (
    -- ** Payload Validation Guards
    vuln797Fix
    , pactBackCompat_v16
    , skipTxTimingValidation
    , enableModuleNameFix
    , enableModuleNameFix2
    , enablePactEvents
    , enableSPVBridge
    , pact4Coin3
    , pact42
    , enforceKeysetFormats
    , doCheckTxHash
    , chainweb213Pact
    , chainweb214Pact
    , chainweb215Pact
    , chainweb216Pact
    , chainweb217Pact
    , chainweb218Pact
    , chainweb219Pact
    , chainweb220Pact
    , chainweb221Pact
    , chainweb222Pact
    , chainweb223Pact
    , chainweb224Pact
    , chainweb225Pact
    , chainweb228Pact
    , chainweb230Pact
    , chainweb231Pact
    , chainweb31
    , chainweb32
    , migratePlatformShare
    , pact5
    , pact44NewTrans
    , maxBlockGasLimit
    , minimumBlockHeaderHistory
    , activeInitialGasModel
    , validPPKSchemes
    , validKeyFormats
    , isWebAuthnPrefixLegal
    , pact4ParserVersion
    , pact5Serialiser

    -- ** BlockHeader Validation Guards
    , slowEpochGuard
    , oldTargetGuard
    , skipFeatureFlagValidationGuard
    , oldDaGuard
    , hashedAdjacentRecord
    ) where

import Chainweb.BlockHeight
import Chainweb.ChainId
import Chainweb.Pact4.Transaction qualified as Pact4
import Chainweb.Utils.Rule
import Chainweb.ForkState
import Chainweb.Version
import Chainweb.Pact.InitialGasModel
import Control.Lens
import Data.Word (Word64)
import Numeric.Natural
import Pact.Core.Builtin qualified as Pact
import Pact.Core.Info qualified as Pact
import Pact.Core.Serialise qualified as Pact
import Pact.Types.KeySet (PublicKeyText, ed25519HexFormat, webAuthnFormat)
import Pact.Types.Scheme (PPKScheme(ED25519, WebAuthn))

getForkHeight :: HasVersion => Fork -> ChainId -> ForkHeight
getForkHeight fork cid = implicitVersion ^?! versionForks . at fork . _Just . atChain cid

-- Check Fork by height
checkFork
    :: HasVersion
    => (ForkHeight -> ForkHeight -> Bool)
    -> Fork -> ChainId -> BlockHeight -> Bool
checkFork p f cid h = p (ForkAtBlockHeight h) (getForkHeight f cid)

-- CheckFork by forkNumber
checkFork'
    :: HasVersion
    => (ForkHeight -> ForkHeight -> Bool)
    -> Fork -> ChainId -> ForkNumber -> Bool
checkFork' p f cid fn = p (ForkAtForkNumber fn) (getForkHeight f cid)


after :: ForkHeight -> ForkHeight -> Bool
after = (>)

atOrAfter :: ForkHeight -> ForkHeight -> Bool
atOrAfter = (>=)

before :: ForkHeight -> ForkHeight -> Bool
before = (<)

-- Intended for forks that intend to run upgrades at exactly one height, and so
-- can't be "pre-activated" for genesis.
atNotGenesis :: ForkHeight -> ForkHeight -> Bool
atNotGenesis _ ForkAtGenesis = error "fork cannot be at genesis"
atNotGenesis fh fh' = fh == fh'

-- -------------------------------------------------------------------------- --
-- Header Validation Guards
--
-- The guards in this section encode when changes to validation rules for blocks
-- on the chain become effective.
--
-- Guards can only take as parameters data that was available when a block was
-- not yet produced. For example, the block creation time, block hash, and nonce
-- of that block are not available, because they were not available before the
-- block was produced.
--
-- In practice, the relevant input data is the `ChainwebVersion`, `ChainId`, and
-- `BlockHeight` of the block under consideration.
--
-- The result is a simple 'Bool'.
--
-- Guards should have meaningful names and should be used in a way that all
-- places in the code base that depend on the guard should reference the
-- respective guard. That way all dependent code can be easily identified using
-- ide tools, like for instance @grep@.
--
-- Each guard should have a description that provides background for the change
-- and provides all information needed for maintaining the code or code that
-- depends on it.
--

-- | Turn off slow epochs (emergency DA) for blocks.
--
-- Emergency DA is considered a misfeature.
--
-- It's intended purpose is to prevent chain hopping attacks, where an attacker
-- temporarily adds a large amount of hash power, thus increasing the
-- difficulty. When the hash power is removed, the remaining hash power may not
-- be enough to reach the next block in reasonable time.
--
-- In practice, emergency DAs cause more problems than they solve. In
-- particular, they increase the chance of deep forks. Also they make the
-- behavior of the system unpredictable in states of emergency, when stability
-- is usually more important than throughput.
--
slowEpochGuard
    :: HasVersion
    => ChainId
    -> BlockHeight
        -- ^ BlockHeight of parent Header
    -> Bool
slowEpochGuard = checkFork before SlowEpoch

-- | Use the current block time for computing epoch start date and
-- target.
--
-- When this guard is switched off, there will be a single epoch of just 119
-- blocks. The target computation won't compensate for that, since the effects
-- are marginal.
--
oldTargetGuard :: HasVersion => ChainId -> BlockHeight -> Bool
oldTargetGuard = checkFork before OldTargetGuard

-- | Skip validation of feature flags.
--
-- Unused feature flag bits are supposed to be set to 0. As of Chainweb 1.7, the
-- Feature Flag bytes and Nonce bytes have switched places in `BlockHeader`. For
-- live chains, enforcing the following condition must be ignored for the
-- historical blocks for which both the Nonce and Flags could be anything.
--
skipFeatureFlagValidationGuard :: HasVersion => ChainId -> BlockHeight -> Bool
skipFeatureFlagValidationGuard = checkFork before SkipFeatureFlagValidation

oldDaGuard :: HasVersion => ChainId -> BlockHeight -> Bool
oldDaGuard = checkFork before OldDAGuard

-----------------
-- Payload validation guards

vuln797Fix :: HasVersion => ChainId -> BlockHeight -> Bool
vuln797Fix = checkFork atOrAfter Vuln797Fix

-- | Preserve Pact bugs pre-1.6 chainweb.
pactBackCompat_v16 :: HasVersion => ChainId -> BlockHeight -> Bool
pactBackCompat_v16 = checkFork before PactBackCompat_v16

-- | Early versions of chainweb used the creation time of the current header
-- for validation of pact tx creation time and TTL. Nowadays the time of
-- the parent header is used.
--
-- When this guard is enabled timing validation is skipped.
--
skipTxTimingValidation :: HasVersion => ChainId -> BlockHeight -> Bool
skipTxTimingValidation = checkFork before SkipTxTimingValidation

-- | Checks height after which module name fix in effect.
--
enableModuleNameFix :: HasVersion => ChainId -> BlockHeight -> Bool
enableModuleNameFix = checkFork atOrAfter ModuleNameFix

-- | Related, later fix (Pact #801).
--
enableModuleNameFix2 :: HasVersion => ChainId -> BlockHeight -> Bool
enableModuleNameFix2 = checkFork atOrAfter ModuleNameFix2

-- | Turn on pact events in command output.
enablePactEvents :: HasVersion => ChainId -> BlockHeight -> Bool
enablePactEvents = checkFork atOrAfter PactEvents

-- | Bridge support: ETH and event SPV.
enableSPVBridge :: HasVersion => ChainId -> BlockHeight -> Bool
enableSPVBridge = checkFork atOrAfter SPVBridge

-- | Should Pact check that keysets have a legal format?
enforceKeysetFormats :: HasVersion => ChainId -> BlockHeight -> Bool
enforceKeysetFormats = checkFork atOrAfter EnforceKeysetFormats

doCheckTxHash :: HasVersion => ChainId -> BlockHeight -> Bool
doCheckTxHash = checkFork atOrAfter CheckTxHash

-- | Fork for musl trans funs
pact44NewTrans :: HasVersion => ChainId -> BlockHeight -> Bool
pact44NewTrans = checkFork atOrAfter Pact44NewTrans

-- | Pact 4.0 features
pact4Coin3 :: HasVersion => ChainId -> BlockHeight -> Bool
pact4Coin3 = checkFork after Pact4Coin3

-- | Pact 4.2 features
pact42 :: HasVersion => ChainId -> BlockHeight -> Bool
pact42 = checkFork atOrAfter Pact42

-- | Pact charges gas for the size of transitive module dependencies
chainweb213Pact :: HasVersion => ChainId -> BlockHeight -> Bool
chainweb213Pact = checkFork atOrAfter Chainweb213Pact

-- | Pact 4.3 features
chainweb214Pact :: HasVersion => ChainId -> BlockHeight -> Bool
chainweb214Pact = checkFork after Chainweb214Pact

-- | Pact 4.3.1 features
chainweb215Pact :: HasVersion => ChainId -> BlockHeight -> Bool
chainweb215Pact = checkFork after Chainweb215Pact

-- | Pact 4.4 features, including new expression parser
chainweb216Pact :: HasVersion => ChainId -> BlockHeight -> Bool
chainweb216Pact = checkFork after Chainweb216Pact

-- | Pact 4.5 features, and considering all table names lowercase
chainweb217Pact :: HasVersion => ChainId -> BlockHeight -> Bool
chainweb217Pact = checkFork after Chainweb217Pact

-- | Pact 4.6 features
chainweb218Pact :: HasVersion => ChainId -> BlockHeight -> Bool
chainweb218Pact = checkFork atOrAfter Chainweb218Pact

-- | Pact 4.7 features + new SPV errors without callstacks
chainweb219Pact :: HasVersion => ChainId -> BlockHeight -> Bool
chainweb219Pact = checkFork atOrAfter Chainweb219Pact

-- | Pact 4.8 features
chainweb220Pact :: HasVersion => ChainId -> BlockHeight -> Bool
chainweb220Pact = checkFork atOrAfter Chainweb220Pact

-- Pact 4.9 features + new spv base64 errors that don't depend on legacy library
-- versions
chainweb221Pact :: HasVersion => ChainId -> BlockHeight -> Bool
chainweb221Pact = checkFork atOrAfter Chainweb221Pact

-- | Pact 4.10 features, webauthn keys and signatures
chainweb222Pact :: HasVersion => ChainId -> BlockHeight -> Bool
chainweb222Pact = checkFork atOrAfter Chainweb222Pact

-- | Pact 4.11 features, verifier plugins
chainweb223Pact :: HasVersion => ChainId -> BlockHeight -> Bool
chainweb223Pact = checkFork atOrAfter Chainweb223Pact

-- | Pact 4.12 features, optimized buy/redeem gas transactions reduce gas cost
-- for all transactions
chainweb224Pact :: HasVersion => ChainId -> BlockHeight -> Bool
chainweb224Pact = checkFork atOrAfter Chainweb224Pact

-- | Update Hyperlane verifier plugin to provide a "merkle tree ISM"
chainweb225Pact :: HasVersion => ChainId -> BlockHeight -> Bool
chainweb225Pact = checkFork atOrAfter Chainweb225Pact

-- | Pact 5, an entirely new Pact compiler and interpreter, and comes with some
-- big changes to PactService
pact5 :: HasVersion => ChainId -> BlockHeight -> Bool
pact5 = checkFork atOrAfter Pact5Fork

-- | Pact 5.1, including a new more succinct serializer for modules
chainweb228Pact :: HasVersion => ChainId -> BlockHeight -> Bool
chainweb228Pact = checkFork atOrAfter Chainweb228Pact

-- | Pact 5.2 and 5.3, including a re-entrancy check for module references
-- Also, a fix for duplicates in `keys`
chainweb230Pact :: HasVersion => ChainId -> BlockHeight -> Bool
chainweb230Pact = checkFork atOrAfter Chainweb230Pact

-- | Pact 5.4, SPV proof root expiry
chainweb231Pact :: HasVersion => ChainId -> BlockHeight -> Bool
chainweb231Pact = checkFork atOrAfter Chainweb231Pact

-- | Fork numbering and voting, continuation proof size gassed
chainweb31 :: HasVersion => ChainId -> BlockHeight -> Bool
chainweb31 = checkFork atOrAfter Chainweb31

-- | Pact 5.4.1
chainweb32 :: HasVersion => ChainId -> ForkNumber -> Bool
chainweb32 = checkFork' atOrAfter Chainweb32

pact4ParserVersion :: HasVersion => ChainId -> BlockHeight -> Pact4.PactParserVersion
pact4ParserVersion cid bh
    | chainweb213Pact cid bh = Pact4.PactParserChainweb213
    | otherwise = Pact4.PactParserGenesis

migratePlatformShare :: HasVersion => ChainId -> BlockHeight -> Bool
migratePlatformShare = checkFork atNotGenesis MigratePlatformShare

pact5Serialiser
    :: HasVersion
    => ChainId
    -> BlockHeight
    -> Pact.PactSerialise Pact.CoreBuiltin Pact.LineInfo
pact5Serialiser cid bh
    | chainweb228Pact cid bh = Pact.serialisePact_lineinfo_pact51
    | otherwise              = Pact.serialisePact_lineinfo_pact50

maxBlockGasLimit
    :: HasVersion
    => ForkNumber
    -> BlockHeight
    -> Maybe Natural
maxBlockGasLimit fn bh = snd $ ruleZipperHere $ snd
    $ ruleSeek (\h _ -> searchKey >= h)
        (_versionMaxBlockGasLimit implicitVersion)
  where
    searchKey = ForkAtBlockHeight bh `max` ForkAtForkNumber fn

minimumBlockHeaderHistory
    :: HasVersion
    => ForkNumber
    -> BlockHeight
    -> Maybe Word64
minimumBlockHeaderHistory fn bh = snd $ ruleZipperHere $ snd
    $ ruleSeek (\h _ -> searchKey >= h)
        (_versionSpvProofRootValidWindow implicitVersion)
  where
    searchKey = ForkAtBlockHeight bh `max` ForkAtForkNumber fn

activeInitialGasModel
    :: HasVersion
    => ChainId
    -> ForkNumber
    -> BlockHeight
    -> InitialGasModel
activeInitialGasModel cid fn bh = snd $ ruleZipperHere $ snd
    $ ruleSeek (\h _ -> searchKey >= h)
        (implicitVersion ^?! versionInitialGasModel . atChain cid)
  where
    searchKey = ForkAtBlockHeight bh `max` ForkAtForkNumber fn

hashedAdjacentRecord :: HasVersion => ChainId -> BlockHeight -> Bool
hashedAdjacentRecord = checkFork atOrAfter HashedAdjacentRecord

-- | Different versions of Chainweb allow different PPKSchemes.
--
validPPKSchemes :: HasVersion => ChainId -> BlockHeight -> [PPKScheme]
validPPKSchemes cid bh =
  if chainweb221Pact cid bh
  then [ED25519, WebAuthn]
  else [ED25519]

validKeyFormats :: HasVersion => ChainId -> BlockHeight -> [PublicKeyText -> Bool]
validKeyFormats cid bh =
  if chainweb222Pact cid bh
  then [ed25519HexFormat, webAuthnFormat]
  else [ed25519HexFormat]

isWebAuthnPrefixLegal :: HasVersion => ChainId -> BlockHeight -> Pact4.IsWebAuthnPrefixLegal
isWebAuthnPrefixLegal cid bh
    | chainweb222Pact cid bh = Pact4.WebAuthnPrefixLegal
    | otherwise = Pact4.WebAuthnPrefixIllegal
