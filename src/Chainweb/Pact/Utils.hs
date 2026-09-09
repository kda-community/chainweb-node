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
    , fromPact4ChainId
    , fromPact5ChainId
    , toTxCreationTime

    -- * k:account helper functions
    , validateKAccount
    , extractPubKeyFromKAccount
    , generateKAccountFromPubKey
    , pubKeyToKAccountKeySet
    , generateKeySetFromKAccount
    , validateKAccountKeySet

    -- * empty payload
    , emptyPayload
    ) where

import Data.Aeson
import qualified Data.Text as T
import qualified Data.Set as S

import Control.Monad.Catch

import qualified Pact.Types.ChainId as Pact4
import qualified Pact.Core.Guards as Pact5
import qualified Pact.Core.ChainData as Pact5

import qualified Pact.JSON.Encode as J

-- Internal modules

import Chainweb.ChainId
import Chainweb.Miner.Pact
import Chainweb.Payload
import Chainweb.Time

fromPactChainId :: MonadThrow m => Pact5.ChainId -> m ChainId
fromPactChainId = fromPact5ChainId

fromPact5ChainId :: MonadThrow m => Pact5.ChainId -> m ChainId
fromPact5ChainId (Pact5.ChainId t) = chainIdFromText t

fromPact4ChainId :: MonadThrow m => Pact4.ChainId -> m ChainId
fromPact4ChainId (Pact4.ChainId t) = chainIdFromText t

-- | This is the recursion principle of an 'Aeson' 'Result' of type 'a'.
-- Similar to 'either', 'maybe', or 'bool' combinators
--
aeson :: (String -> b) -> (a -> b) -> Result a -> b
aeson f _ (Error a) = f a
aeson _ g (Success a) = g a

toTxCreationTime :: Time Micros -> Pact5.TxCreationTime
toTxCreationTime (Time timespan) =
  Pact5.TxCreationTime $ fromIntegral $ timeSpanToSeconds timespan



validateKAccount :: T.Text -> Bool
validateKAccount acctName =
  case T.take 2 acctName of
    "k:" ->
      let pubKey = Pact5.PublicKeyText $ T.drop 2 acctName
      in Pact5.ed25519HexFormat pubKey
    _ -> False

extractPubKeyFromKAccount :: T.Text -> Maybe Pact5.PublicKeyText
extractPubKeyFromKAccount kacct
  | validateKAccount kacct =
    Just $ Pact5.PublicKeyText $ T.drop 2 kacct
  | otherwise = Nothing

generateKAccountFromPubKey :: Pact5.PublicKeyText -> Maybe T.Text
generateKAccountFromPubKey pubKey
  | Pact5.ed25519HexFormat pubKey =
    let pubKeyText = Pact5._pubKey pubKey
    in Just $ "k:" <> pubKeyText
  | otherwise = Nothing


-- Warning: Only use if already certain that PublicKeyText
-- is valid.
-- Note: We are assuming the k: account is ED25519.
pubKeyToKAccountKeySet :: Pact5.PublicKeyText -> Pact5.KeySet
pubKeyToKAccountKeySet pubKey = Pact5.KeySet (S.singleton pubKey) Pact5.KeysAll

generateKeySetFromKAccount :: T.Text -> Maybe Pact5.KeySet
generateKeySetFromKAccount kacct = do
  pubKey <- extractPubKeyFromKAccount kacct
  pure $ pubKeyToKAccountKeySet pubKey

validateKAccountKeySet :: T.Text -> Pact5.KeySet -> Bool
validateKAccountKeySet kacct actualKeySet =
  case generateKeySetFromKAccount kacct of
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
