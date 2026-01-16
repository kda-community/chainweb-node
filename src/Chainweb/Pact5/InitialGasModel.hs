{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}

module Chainweb.Pact5.InitialGasModel
  ( InitialGasModel(..)
  , pre31GasModel
  , post31GasModel
  , post32GasModel
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
import Control.Lens


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
                        ED25519 -> 10.0     -- | TODO => Needs to be benchmarked
                        WebAuthn -> 10.0    -- |
  }
