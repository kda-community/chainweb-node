{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Chainweb.TransactionHash
  (TransactionHash(..)
  ) where

import GHC.Generics

import Control.DeepSeq (NFData)
import Control.Exception
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Short as SB
import Data.Text (Text)
import qualified Data.Text as T

import Data.Aeson
import Data.Hashable (Hashable(hashWithSalt))

import qualified Pact.JSON.Encode as J

import Chainweb.Utils
import Chainweb.Utils.Serialization

------------------------------------------------------------------------------
-- | Raw/unencoded transaction hashes.
--
-- TODO: production versions of this kind of DB should salt with a
-- runtime-generated constant to avoid collision attacks; see the \"hashing and
-- security\" section of the hashable docs.
newtype TransactionHash = TransactionHash { unTransactionHash :: SB.ShortByteString }
  deriving stock (Read, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Show TransactionHash where
    show = T.unpack . encodeToText

instance Hashable TransactionHash where
  hashWithSalt s (TransactionHash h) = hashWithSalt s (hashCode :: Int)
    where
      hashCode = either error id $ runGetEitherS (fromIntegral <$> getWord64le) (B.take 8 $ SB.fromShort h)
  {-# INLINE hashWithSalt #-}

instance ToJSON TransactionHash where
  toJSON = toJSON . toText
  {-# INLINE toJSON #-}

instance J.Encode TransactionHash where
  build = J.text . toText
  {-# INLINE build #-}

instance FromJSON TransactionHash where
  parseJSON = withText "TransactionHash" (either (fail . show) return . p)
    where
      p :: Text -> Either SomeException TransactionHash
      !p = (TransactionHash . SB.toShort <$>) . decodeB64UrlNoPaddingText

instance HasTextRepresentation TransactionHash where
  toText (TransactionHash th) = encodeB64UrlNoPaddingText $ SB.fromShort th
  fromText = (TransactionHash . SB.toShort <$>) . decodeB64UrlNoPaddingText
  {-# INLINE toText #-}
  {-# INLINE fromText #-}
