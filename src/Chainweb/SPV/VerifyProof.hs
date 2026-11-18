{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Chainweb.SPV.VerifyProof
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Verification of Merkle proofs in the Chainweb Merkle tree.
--
module Chainweb.SPV.VerifyProof
( runTransactionOutputProof
, checkProofAndExtractOutput
) where

import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Text (Text)

import Data.MerkleLog.V1 qualified as V1

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeight (BlockHeight)
import Chainweb.ChainId (ChainId)
import Chainweb.Crypto.MerkleLog (proofSubject)
import Chainweb.MerkleLogHash
import Chainweb.MerkleUniverse
import Chainweb.Pact.Payload
import Chainweb.SPV
import Chainweb.Pact.Backend.Types (HeaderOracle (..))
import Chainweb.Parent
import Chainweb.Utils (unlessM)
import Chainweb.Version qualified as CW
import Chainweb.Version.Guards qualified as CW

-- -------------------------------------------------------------------------- --

-- | Runs a transaction Proof. Returns the block hash on the target chain for
-- which inclusion is proven.
--
runTransactionOutputProof
    :: MonadThrow m
    => TransactionOutputProof ChainwebMerkleHashAlgorithm
    -> m BlockHash
runTransactionOutputProof (TransactionOutputProof _ p)
    = BlockHash . MerkleLogHash <$> V1.runMerkleProof p

-- | The error message for an SPV proof whose target header lies outside of the
-- bounds of the oracle. This happens when the target has been forked out of the
-- chain, or -- since the introduction of the SPV proof expiration window at the
-- 'Chainweb231Pact' fork -- when the target is too old and has fallen outside
-- of the oracle's bounds.
--
-- This corresponds to the @forkedError@ used by the Pact 4 SPV support on
-- master. Unlike on master, the message is returned as a 'Text' error rather
-- than thrown as a 'SpvException', and it already includes the
-- @spv verification failed:@ prefix, because on this branch the error is
-- surfaced directly by 'checkProofAndExtractOutput'.
forkedError :: CW.HasVersion => ChainId -> BlockHeight -> Text
forkedError cid h =
    "spv verification failed: " <>
    if CW.chainweb231Pact cid h
    then "target header is not in the chain or is out of bounds"
    else "target header is not in the chain"

checkProofAndExtractOutput
    :: CW.HasVersion
    => HeaderOracle
    -> BlockHeight
    -> TransactionOutputProof ChainwebMerkleHashAlgorithm
    -> ExceptT Text IO TransactionOutput
checkProofAndExtractOutput oracle blockHeight proof@(TransactionOutputProof _cid p) = do
    h <- runTransactionOutputProof proof
    unlessM (liftIO $ oracle.consult (Parent h)) $
        throwError $ forkedError (oracle.chain) blockHeight
    proofSubject p
