{-# language DeriveAnyClass #-}
{-# LANGUAGE BangPatterns #-}
{-# language DeriveFunctor #-}
{-# language DeriveGeneric #-}
{-# language DeriveTraversable #-}
{-# language DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# language FlexibleContexts #-}
{-# language ImportQualifiedPost #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language PackageImports #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}

module Chainweb.Pact5.Transaction
  ( Transaction
  , PayloadWithText
  , UnparsedTransaction
  , HashableTrans(..)
  , mkPayloadWithText
  , cmdGasLimit
  , cmdGasPrice
  , cmdTimeToLive
  , cmdCreationTime
  , payloadBytes
  , payloadObj
  , payloadCodec
  , parseCommand
  , parseTransaction
  , parsePact4Command
  , rawCommandCodec
  , toGasLimit
  , fromGasLimit
  , requestKeyToTransactionHash
  ) where

import Data.Hashable

import Data.Coerce (coerce)
import "aeson" Data.Aeson qualified as Aeson
import "base" Data.Function
import "base" GHC.Generics (Generic)
import "bytestring" Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import "bytestring" Data.ByteString.Short qualified as SB
import "deepseq" Control.DeepSeq
import "lens" Control.Lens
import "pact-json" Pact.JSON.Encode qualified as J
import "pact-json" Pact.JSON.Encode (Encode(..))
import "pact-tng" Pact.Core.ChainData
import "pact-tng" Pact.Core.Command.Types
import "pact-tng" Pact.Core.Errors
import "pact-tng" Pact.Core.Gas
import "pact-tng" Pact.Core.Hash
import "pact-tng" Pact.Core.Info
import "pact-tng" Pact.Core.Pretty qualified as Pact5
import "text" Data.Text (Text)
import "text" Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Chainweb.Pact4.Transaction qualified as Pact4
import Chainweb.Utils
import Chainweb.TransactionHash
import Chainweb.Utils.Serialization

type Transaction = Command (PayloadWithText PublicMeta ParsedCode)

type UnparsedTransaction = Command (PayloadWithText PublicMeta Text)

data PayloadWithText meta code = UnsafePayloadWithText
    { _payloadBytes :: !SB.ShortByteString
    , _payloadObj :: !(Payload meta code)
    }
    deriving stock (Show, Generic)
    deriving stock (Functor, Traversable, Foldable)
    deriving anyclass (NFData)

instance Eq (PayloadWithText meta code) where
    (==) = (==) `on` _payloadBytes

instance (J.Encode meta, J.Encode code) => J.Encode (PayloadWithText meta code) where
    build p = J.object
      [ "payloadBytes" J..= J.encodeText (decodeUtf8 $ SB.fromShort $ _payloadBytes p)
      , "payloadObject" J..= _payloadObj p
      ]

payloadBytes :: Getter (PayloadWithText meta code) SB.ShortByteString
payloadBytes = to _payloadBytes
{-# inline conlike payloadBytes #-}

payloadObj :: Getter (PayloadWithText meta code) (Payload meta code)
payloadObj = to _payloadObj
{-# inline conlike payloadObj #-}

mkPayloadWithText :: Command (ByteString, Payload meta code) -> Command (PayloadWithText meta code)
mkPayloadWithText = over cmdPayload $ \(bs, p) -> UnsafePayloadWithText
    { _payloadBytes = SB.toShort bs
    , _payloadObj = p
    }

-- | Hashable newtype of Transaction
newtype HashableTrans a = HashableTrans { unHashable :: Command a }
    deriving (Eq, Functor, Ord)

instance (Eq code, Eq meta) => Hashable (HashableTrans (PayloadWithText meta code)) where
    hashWithSalt s (HashableTrans t) = hashWithSalt s hashCode
      where
        hc = unHash $ _cmdHash t
        decHC = runGetEitherS getWord64le
        !hashCode = either error id $ decHC (B.take 8 $ SB.fromShort hc)
    {-# INLINE hashWithSalt #-}

rawCommandCodec :: Codec UnparsedTransaction
rawCommandCodec = Codec enc dec
    where
    enc cmd = J.encodeStrict $ J.text . decodeUtf8 . SB.fromShort . _payloadBytes <$> cmd
    dec bs = do
        cmd' :: (Command Text) <- Aeson.eitherDecodeStrict' bs
        let p = encodeUtf8 $ _cmdPayload cmd'
        payloadObject <- over (_Right . pMeta) _stableEncoding $ Aeson.eitherDecodeStrict' p
        let payloadWithText = UnsafePayloadWithText { _payloadBytes = SB.toShort p, _payloadObj = payloadObject }
        return $ payloadWithText <$ cmd'

-- | A codec for Pact5's (Command PayloadWithText) transactions.
--
payloadCodec
    :: Codec (Command (PayloadWithText PublicMeta ParsedCode))
payloadCodec = Codec enc dec
    where
    enc c = J.encodeStrict $ fmap (decodeUtf8 . encodePayload) c
    dec bs = case Aeson.decodeStrict' bs of
        -- Note: this can only ever emit a `ParseError`, which by default are quite small.
        -- Still, `pretty` instances are scary, but this cannot make it into block outputs so this should
        -- be okay
        Just (cmd :: Command Text) -> over _Left Pact5.renderCompactString $ parseCommand cmd
        Nothing -> Left "decode PayloadWithText failed"

parseCommand :: Command Text -> Either (PactError SpanInfo) Transaction
parseCommand cmd = do
    let cmd' = fmap encodeUtf8 cmd
    let code = SB.toShort (_cmdPayload cmd')
    parsedCmd <- over (_Right . cmdPayload . pMeta) _stableEncoding $ unsafeParseCommand cmd'
    return (parsedCmd & cmdPayload %~ \obj -> UnsafePayloadWithText { _payloadBytes = code, _payloadObj = obj })

parseTransaction :: UnparsedTransaction -> Either (PactError SpanInfo) Transaction
parseTransaction = traverse (traverse parsePact)


encodePayload :: PayloadWithText meta code -> ByteString
encodePayload = SB.fromShort . _payloadBytes


-- TODO Remove this when possible ... That's ugly
parsePact4Command :: Pact4.UnparsedTransaction -> Either (Either Text (PactError SpanInfo)) Transaction
parsePact4Command bs =
  case Aeson.decodeStrict' (codecEncode Pact4.rawCommandCodec bs) of
      -- Note: this can only ever emit a `ParseError`, which by default are quite small.
      -- Still, `pretty` instances are scary, but this cannot make it into block outputs so this should
      -- be okay
      Just (cmd :: Command Text) -> over _Left Right $ parseCommand cmd
      Nothing -> Left $ Left "decode PayloadWithText failed"

-- decodePayload
--     :: ByteString
--     -> Either String PayloadWithText
-- decodePayload bs = case Aeson.decodeStrict' bs of
--     Just (payload :: Payload (StableEncoding PublicMeta) Text) -> do
--         p <- traverse parseCode $
--             over pMeta _stableEncoding payload
--         return $! PayloadWithText (SB.toShort bs) p
--     Nothing -> Left "decoding Payload failed"

-- | Access the gas limit/supply of a public chain command payload
cmdGasLimit :: Lens' (Command (Payload PublicMeta c)) GasLimit
cmdGasLimit = cmdPayload . pMeta . pmGasLimit
{-# INLINE cmdGasLimit #-}

-- | Get the gas price of a public chain command payload
cmdGasPrice :: Lens' (Command (Payload PublicMeta c)) GasPrice
cmdGasPrice = cmdPayload . pMeta . pmGasPrice
{-# INLINE cmdGasPrice #-}

cmdTimeToLive :: Lens' (Command (Payload PublicMeta c)) TTLSeconds
cmdTimeToLive = cmdPayload . pMeta . pmTTL
{-# INLINE cmdTimeToLive #-}

cmdCreationTime :: Lens' (Command (Payload PublicMeta c)) TxCreationTime
cmdCreationTime = cmdPayload . pMeta . pmCreationTime
{-# INLINE cmdCreationTime #-}


toGasLimit:: Integral a => a -> GasLimit
toGasLimit = GasLimit . Gas . fromIntegral

fromGasLimit:: Integral a => GasLimit -> a
fromGasLimit = fromIntegral . _gas . coerce

requestKeyToTransactionHash :: RequestKey -> TransactionHash
requestKeyToTransactionHash = TransactionHash . unHash . unRequestKey