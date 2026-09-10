{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Chainweb.Pact.Utils
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Mark Nichols <mark@kadena.io>
-- Stability: experimental
--
-- Pact service for Chainweb

module Chainweb.Pact.Utils
    ( -- * combinators
      aeson
    , fromPactChainId
    , toTxCreationTime

    -- * k:account helper functions
    , validateKAccount
    , extractPubKeyFromKAccount
    , generateKAccountFromPubKey
    , pubKeyToKAccountKeySet
    , generateKeySetFromKAccount
    , validateKAccountKeySet

    -- * q:account helper functions (NIST FIPS 205 SLH-DSA Post-Quantum, contributed by not_bob & seal_klub)
    , validateQAccount
    , extractPubKeyFromQAccount
    , generateQAccountFromPubKey
    , pubKeyToQAccountKeySet
    , generateKeySetFromQAccount
    , validateQAccountKeySet

    -- * empty payload
    , emptyPayload
    ) where

import Data.Aeson
import qualified Data.Text as T

import Control.Monad.Catch

import Pact.Parse
import qualified Pact.Types.ChainId as P
import qualified Pact.Types.Term as P
import Pact.Types.ChainMeta
import Pact.Types.KeySet (ed25519HexFormat)

import qualified Pact.JSON.Encode as J

-- Internal modules

import Chainweb.ChainId
import Chainweb.Miner.Pact
import Chainweb.Payload
import Chainweb.Time

fromPactChainId :: MonadThrow m => P.ChainId -> m ChainId
fromPactChainId (P.ChainId t) = chainIdFromText t

-- | This is the recursion principle of an 'Aeson' 'Result' of type 'a'.
-- Similar to 'either', 'maybe', or 'bool' combinators
--
aeson :: (String -> b) -> (a -> b) -> Result a -> b
aeson f _ (Error a) = f a
aeson _ g (Success a) = g a

toTxCreationTime :: Time Micros -> TxCreationTime
toTxCreationTime (Time timespan) =
  TxCreationTime $ ParsedInteger $ fromIntegral $ timeSpanToSeconds timespan



validateKAccount :: T.Text -> Bool
validateKAccount acctName =
  case T.take 2 acctName of
    "k:" ->
      let pubKey = P.PublicKeyText $ T.drop 2 acctName
      in ed25519HexFormat pubKey
    _ -> False

extractPubKeyFromKAccount :: T.Text -> Maybe P.PublicKeyText
extractPubKeyFromKAccount kacct
  | validateKAccount kacct =
    Just $ P.PublicKeyText $ T.drop 2 kacct
  | otherwise = Nothing

generateKAccountFromPubKey :: P.PublicKeyText -> Maybe T.Text
generateKAccountFromPubKey pubKey
  | ed25519HexFormat pubKey =
    let pubKeyText = P._pubKey pubKey
    in Just $ "k:" <> pubKeyText
  | otherwise = Nothing


-- Warning: Only use if already certain that PublicKeyText
-- is valid.
-- Note: We are assuming the k: account is ED25519.
pubKeyToKAccountKeySet :: P.PublicKeyText -> P.KeySet
pubKeyToKAccountKeySet pubKey = P.mkKeySet [pubKey] "keys-all"

generateKeySetFromKAccount :: T.Text -> Maybe P.KeySet
generateKeySetFromKAccount kacct = do
  pubKey <- extractPubKeyFromKAccount kacct
  pure $ pubKeyToKAccountKeySet pubKey

validateKAccountKeySet :: T.Text -> P.KeySet -> Bool
validateKAccountKeySet kacct actualKeySet =
  case generateKeySetFromKAccount kacct of
    Nothing -> False
    Just expectedKeySet
      | expectedKeySet == actualKeySet -> True
      | otherwise -> False

-- =============================================================================
-- Post-Quantum (NIST FIPS 205 SLH-DSA) q: Account Helpers
-- Contributed by not_bob & seal_klub
-- =============================================================================

validateQAccount :: T.Text -> Bool
validateQAccount acctName =
  case T.take 2 acctName of
    "q:" ->
      let pubKey = T.drop 2 acctName
      in T.length pubKey >= 64 && T.all (\c -> (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')) pubKey
    _ -> False

extractPubKeyFromQAccount :: T.Text -> Maybe P.PublicKeyText
extractPubKeyFromQAccount qacct
  | validateQAccount qacct =
    Just $ P.PublicKeyText $ T.drop 2 qacct
  | otherwise = Nothing

generateQAccountFromPubKey :: P.PublicKeyText -> Maybe T.Text
generateQAccountFromPubKey pubKey =
  let pubKeyText = P._pubKey pubKey
  in if T.length pubKeyText >= 64
     then Just $ "q:" <> pubKeyText
     else Nothing

pubKeyToQAccountKeySet :: P.PublicKeyText -> P.KeySet
pubKeyToQAccountKeySet pubKey = P.mkKeySet [pubKey] "keys-all"

generateKeySetFromQAccount :: T.Text -> Maybe P.KeySet
generateKeySetFromQAccount qacct = do
  pubKey <- extractPubKeyFromQAccount qacct
  pure $ pubKeyToQAccountKeySet pubKey

validateQAccountKeySet :: T.Text -> P.KeySet -> Bool
validateQAccountKeySet qacct actualKeySet =
  case generateKeySetFromQAccount qacct of
    Nothing -> False
    Just expectedKeySet
      | expectedKeySet == actualKeySet -> True
      | otherwise -> False

-- | Empty payload marking no-op transaction payloads.
--
emptyPayload :: PayloadWithOutputs
emptyPayload = newPayloadWithOutputs miner coinbase mempty
  where
    miner = MinerData $ J.encodeStrict noMiner
    coinbase = noCoinbaseOutput
