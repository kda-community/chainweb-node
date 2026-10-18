{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NumericUnderscores #-}

module Chainweb.Pact5.InitialGasModel
  ( InitialGasModel(..)
  , pre31GasModel
  , post31GasModel
  , post32GasModel
  , post33GasModel
  -- Lenses
  , feePerByte
  , rawPayloadSizeFactor
  , proofSizeFactor
  , signatureSizeFactor
  , sizePenalty
  , signatureCost
  ) where

import Control.DeepSeq
import Pact.Core.Scheme
import Pact.Core.Gas
import Control.Lens

absoluteGasLimit :: Rational
absoluteGasLimit = fromIntegral $ _gas $ maxBound

data InitialGasModel = InitialGasModel
  { _feePerByte :: Rational
      -- ^ Base Price charged per byte
  , _rawPayloadSizeFactor :: Rational
      -- ^ Multiplier for the raw payload (without continuation proof) size
  , _proofSizeFactor :: Rational
      -- ^ Multiplier for the proof size
  , _signatureSizeFactor :: Rational
      -- ^ Multiplier for signatures size
  , _sizePenalty :: Rational -> Rational
      -- ^ Function used to compute a penalty for big transactions
  , _signatureCost :: PPKScheme -> Rational
      -- ^ Function used to compute a fixed amount of gas per signature
  }

-- Required to be used as a rule
instance NFData InitialGasModel where
  rnf (InitialGasModel {}) = ()

makeLenses ''InitialGasModel

pre31GasModel :: InitialGasModel
pre31GasModel = InitialGasModel
  { _feePerByte = 0.01
  , _rawPayloadSizeFactor = 1.0
  , _proofSizeFactor = 0.0
  , _signatureSizeFactor = 0.0
  , _sizePenalty = \x -> (x / 512) ^ (7 :: Integer)
  , _signatureCost = const 0.0
  }


post31GasModel :: InitialGasModel
post31GasModel = InitialGasModel
  { _feePerByte = 0.01
  , _rawPayloadSizeFactor = 1.0
  , _proofSizeFactor = 1.0
  , _signatureSizeFactor = 0.0
  , _sizePenalty = \x -> (x / 512) ^ (7 :: Integer)
  , _signatureCost = const 0.0
  }


post32GasModel :: InitialGasModel
post32GasModel = InitialGasModel
  { _feePerByte = 0.01
  , _rawPayloadSizeFactor = 1.0
  , _proofSizeFactor = 1.0
  , _signatureSizeFactor = 1.0
  , _sizePenalty = \x -> (x / 512) ^ (7 :: Integer)
  , _signatureCost = \case
                        ED25519 ->   21.0        -- | Benchmarked at 52 ns
                        WebAuthn -> 526.0        -- | Benchmarked at 1.315 ms (worst case)
                        _ -> absoluteGasLimit    -- | Make sure the transaction will fail for unsupported schemes => Should never happen
  }


post33GasModel :: InitialGasModel
post33GasModel = InitialGasModel
  { _feePerByte = 0.01
  , _rawPayloadSizeFactor = 1.0
  , _proofSizeFactor = 1.0
  , _signatureSizeFactor = 1.0
  , _sizePenalty = \x -> (x / 512) ^ (7 :: Integer)
  , _signatureCost = \case
                        ED25519 ->   21.0        -- | Benchmarked at 52 ns
                        WebAuthn -> 526.0        -- | Benchmarked at 1.315 ms (worst case)
                        SlhDsaSha128s -> 816.0   -- | Becnhmarked at 2.04 ms with OpenSSL backend
                        SlhDsaSha192s -> 1584.0  -- | Benchmarked at 3.96 ms with OpenSSL backend
                        SlhDsaSha256s -> 3992.0  -- | Benchmarked at 9.98 ms with OpenSSL backend
  }
