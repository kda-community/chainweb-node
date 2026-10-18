{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module: Chainweb.Pact.PactService.Pact4.ExecBlock
-- Copyright: Copyright © 2020 Kadena LLC.
-- License: See LICENSE file
-- Maintainers: Lars Kuhtz, Emily Pillmore, Stuart Popejoy
-- Stability: experimental
--
-- Functionality for playing block transactions.
--
module Chainweb.Pact.PactService.Pact4.ExecBlock
    ( execBlock
    , execTransactions
    , toPayloadWithOutputs
    , validateParsedChainwebTx
    , validateRawChainwebTx
    , validateHashes
    , throwCommandInvalidError
    , initModuleCacheForBlock
    , runCoinbase
    , CommandInvalidError(..)
    , checkParse
    ) where

import Chronos qualified
import Control.Concurrent.MVar
import Control.DeepSeq
import Control.Exception (evaluate)
import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State.Strict

import System.LogLevel (LogLevel(..))
import qualified Data.ByteString.Short as SB
import Data.List qualified as List
import Data.Either
import Data.Foldable (toList)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V

import System.IO
import System.Timeout

import Prelude hiding (lookup)

import Pact.Compile (compileExps)
import Pact.Interpreter(PactDbEnv(..))
import qualified Pact.JSON.Encode as J
import qualified Pact.Parse as Pact4 hiding (parsePact)
import qualified Pact.Types.Command as Pact4
import Pact.Types.ExpParser (mkTextInfo, ParseEnv(..))
import qualified Pact.Types.Hash as Pact4
import Pact.Types.RPC
import qualified Pact.Types.Runtime as Pact4
import qualified Pact.Types.SPV as Pact4

import Chainweb.BlockHeader
import Chainweb.BlockHeight
import Chainweb.Logger
import Chainweb.Mempool.Mempool as Mempool
import Chainweb.MinerReward
import Chainweb.Miner.Pact

import Chainweb.Pact.Types
import Chainweb.Pact4.SPV qualified as Pact4
import Chainweb.Pact4.NoCoinbase
import qualified Chainweb.Pact4.Transaction as Pact4
import qualified Chainweb.Pact4.TransactionExec as Pact4
import qualified Chainweb.Pact4.Validations as Pact4
import qualified Pact.Core.Gas as Pact5
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.Time
import Chainweb.Utils hiding (check)
import Chainweb.Version
import Chainweb.ForkState (pact4ForkNumber)
import Chainweb.Version.Guards
import Chainweb.Pact4.Backend.ChainwebPactDb
import Data.Coerce
import Chainweb.Pact4.Types
import Chainweb.Pact4.ModuleCache
import Control.Monad.Except
import qualified Data.List.NonEmpty as NE


-- | Execute a block -- only called in validate either for replay or for validating current block.
--
execBlock
    :: (CanReadablePayloadCas tbl, Logger logger)
    => BlockHeader
        -- ^ this is the current header. We may consider changing this to the parent
        -- header to avoid confusion with new block and prevent using data from this
        -- header when we should use the respective values from the parent header
        -- instead.
    -> CheckablePayload
    -> PactBlockM logger tbl (Pact4.Gas, PayloadWithOutputs)
execBlock currHeader payload = do
    let plData = checkablePayloadToPayloadData payload
    dbEnv <- view psBlockDbEnv
    miner <- decodeStrictOrThrow' (_minerData $ view payloadDataMiner plData)

    trans <- liftIO $ pact4TransactionsFromPayload
      (pact4ParserVersion v (_chainId currHeader) (view blockHeight currHeader))
      plData
    logger <- view (psServiceEnv . psLogger)

    -- The reference time for tx timings validation.
    --
    -- The legacy behavior is to use the creation time of the /current/ header.
    -- The new default behavior is to use the creation time of the /parent/ header.
    --
    txValidationTime <- if isGenesisBlockHeader currHeader
      then return (ParentCreationTime $ view blockCreationTime currHeader)
      else ParentCreationTime . view blockCreationTime . _parentHeader <$> view psParentHeader

    -- prop_tx_ttl_validate
    errorsIfPresent <- liftIO $
        forM (V.toList trans) $ \tx ->
          fmap (Pact4._cmdHash tx,) $
            runExceptT $
              validateParsedChainwebTx logger v cid dbEnv txValidationTime
                (view blockHeight currHeader) tx

    case NE.nonEmpty [ (hsh, sshow err) | (hsh, Left err) <- errorsIfPresent ] of
      Nothing -> return ()
      Just errs -> throwM $ Pact4TransactionValidationException errs

    logInitCache

    !results <- go miner trans >>= throwCommandInvalidError

    let !totalGasUsed = sumOf (folded . to Pact4._crGas) results

    pwo <- either throwM return $
      validateHashes currHeader payload miner results
    return (totalGasUsed, pwo)
  where
    blockGasLimit =
      fromIntegral <$> maxBlockGasLimit v pact4ForkNumber (view blockHeight currHeader)

    logInitCache = liftPactServiceM $ do
      mc <- fmap (fmap instr . _getModuleCache) <$> use psInitCache
      logDebugPact $ "execBlock: initCache: " <> sshow mc

    instr (md,_) = preview (Pact4._MDModule . Pact4.mHash) $ Pact4._mdModule md

    v = _chainwebVersion currHeader
    cid = _chainId currHeader

    isGenesisBlock = isGenesisBlockHeader currHeader

    go m txs = if isGenesisBlock
      then
        -- GENESIS VALIDATE COINBASE: Reject bad coinbase, use date rule for precompilation
        execTransactions m txs
          (EnforceCoinbaseFailure True) (CoinbaseUsePrecompiled False) blockGasLimit Nothing
      else
        -- VALIDATE COINBASE: back-compat allow failures, use date rule for precompilation
        execTransactions m txs
          (EnforceCoinbaseFailure False) (CoinbaseUsePrecompiled False) blockGasLimit Nothing

throwCommandInvalidError
    :: Transactions Pact4 (Either CommandInvalidError a)
    -> PactBlockM logger tbl (Transactions Pact4 a)
throwCommandInvalidError = (transactionPairs . traverse . _2) throwGasFailure
  where
    throwGasFailure = \case
      Left (CommandInvalidGasPurchaseFailure e) -> throwM (Pact4BuyGasFailure e)

      -- this should be impossible because we don't
      -- set tx time limits in validateBlock
      Left (CommandInvalidTxTimeout t) -> throwM t

      Right r -> pure r

-- | The validation logic for Pact Transactions that have not had their
-- code parsed yet. This is used by the mempool to estimate tx validity
-- before inclusion into blocks, but it's also used by ExecBlock to check
-- if all of the txs in a block are valid.
--
-- Skips validation for genesis transactions, since gas accounts, etc. don't
-- exist yet.
--
validateRawChainwebTx
    :: forall logger
    . (Logger logger)
    => logger
    -> ChainwebVersion
    -> ChainId
    -> PactDbFor logger Pact4
    -> ParentCreationTime
        -- ^ reference time for tx validation.
    -> BlockHeight
        -- ^ Current block height
    -> Pact4.UnparsedTransaction
    -> ExceptT InsertError IO Pact4.Transaction
validateRawChainwebTx logger v cid dbEnv txValidationTime bh tx = do
  parsed <- checkParse logger v cid bh tx
  validateParsedChainwebTx logger v cid dbEnv txValidationTime bh parsed
  return parsed

-- | The principal validation logic for groups of Pact Transactions.
-- This is used by the mempool to estimate tx validity
-- before inclusion into blocks, but it's also used by ExecBlock to check
-- if all of the txs in a block are valid.
--
-- Skips validation for genesis transactions, since gas accounts, etc. don't
-- exist yet.
--
validateParsedChainwebTx
    :: Logger logger
    => logger
    -> ChainwebVersion
    -> ChainId
    -> PactDbFor logger Pact4
    -> ParentCreationTime
        -- ^ reference time for tx validation.
    -> BlockHeight
        -- ^ Current block height
    -> Pact4.Transaction
    -> ExceptT InsertError IO ()
validateParsedChainwebTx logger v cid dbEnv txValidationTime bh tx
  | bh == genesisHeight v cid = pure ()
  | otherwise = do
      checkUnique logger dbEnv tx
      checkTxHash logger v cid bh tx
      checkChain cid tx
      checkTxSigs logger v cid bh tx
      checkTimes logger v cid bh txValidationTime tx
      _ <- checkCompile logger v cid bh tx
      return ()

checkChain
  :: ChainId -> Pact4.Transaction -> ExceptT InsertError IO ()
checkChain cid tx =
  unless (Pact4.assertChainId cid txCid) $
    throwError $ InsertErrorWrongChain (chainIdToText cid) (Pact4._chainId txCid)
  where
  txCid = view (Pact4.cmdPayload . to Pact4.payloadObj . Pact4.pMeta . Pact4.pmChainId) tx

checkUnique
  :: (Logger logger)
  => logger
  -> PactDbFor logger Pact4
  -> Pact4.Command (Pact4.PayloadWithText meta code)
  -> ExceptT InsertError IO ()
checkUnique logger dbEnv t = do
  liftIO $ logFunctionText logger Debug $ "Pact4.checkUnique: " <> sshow (Pact4._cmdHash t)
  found <- liftIO $
    HashMap.lookup (coerce $ Pact4.toUntypedHash $ Pact4._cmdHash t) <$>
      _cpLookupProcessedTx dbEnv
        (V.singleton $ coerce $ Pact4.toUntypedHash $ Pact4._cmdHash t)
  case found of
    Nothing -> pure ()
    Just _ -> throwError InsertErrorDuplicate

checkTimes
  :: (Logger logger)
  => logger
  -> ChainwebVersion
  -> ChainId
  -> BlockHeight
  -> ParentCreationTime
  -> Pact4.Command (Pact4.PayloadWithText Pact4.PublicMeta code)
  -> ExceptT InsertError IO ()
checkTimes logger v cid bh txValidationTime t = do
    liftIO $ logFunctionText logger Debug $ "Pact4.checkTimes: " <> sshow (Pact4._cmdHash t)
    if | skipTxTimingValidation v cid bh ->
           return ()
       | not (Pact4.assertTxNotInFuture txValidationTime (Pact4.payloadObj <$> t)) ->
           throwError InsertErrorTimeInFuture
       | not (Pact4.assertTxTimeRelativeToParent txValidationTime (Pact4.payloadObj <$> t)) ->
           throwError InsertErrorTTLExpired
       | otherwise ->
           return ()

checkTxHash
  :: (Logger logger)
  => logger
  -> ChainwebVersion
  -> ChainId
  -> BlockHeight
  -> Pact4.Command (Pact4.PayloadWithText Pact4.PublicMeta code)
  -> ExceptT InsertError IO ()
checkTxHash logger v cid bh t = do
    liftIO $ logFunctionText logger Debug $ "Pact4.checkTxHash: " <> sshow (Pact4._cmdHash t)
    case Pact4.verifyHash (Pact4._cmdHash t) (SB.fromShort $ Pact4.payloadBytes $ Pact4._cmdPayload t) of
        Left _
            | doCheckTxHash v cid bh -> throwError InsertErrorInvalidHash
            | otherwise -> pure ()
        Right _ -> pure ()

checkTxSigs
  :: (MonadIO f, MonadError InsertError f, Logger logger)
  => logger
  -> ChainwebVersion
  -> ChainId
  -> BlockHeight
  -> Pact4.Command (Pact4.PayloadWithText m c)
  -> f ()
checkTxSigs logger v cid bh t = do
  liftIO $ logFunctionText logger Debug $ "Pact4.checkTxSigs: " <> sshow (Pact4._cmdHash t)
  case Pact4.assertValidateSigs validSchemes webAuthnPrefixLegal hsh signers sigs of
      Right _ -> do
          pure ()
      Left err -> do
        throwError $ InsertErrorInvalidSigs (displayAssertValidateSigsError err)
  where
    hsh = Pact4._cmdHash t
    sigs = Pact4._cmdSigs t
    signers = Pact4._pSigners $ Pact4.payloadObj $ Pact4._cmdPayload t
    validSchemes = validPPKSchemes v cid bh
    webAuthnPrefixLegal = isWebAuthnPrefixLegal v cid bh

checkCompile
  :: (Logger logger)
  => logger
  -> ChainwebVersion
  -> ChainId
  -> BlockHeight
  -> Pact4.Command (Pact4.PayloadWithText Pact4.PublicMeta Pact4.ParsedCode)
  -> ExceptT InsertError IO Pact4.Transaction
checkCompile logger v cid bh tx = do
  liftIO $ logFunctionText logger Debug $ "Pact4.checkCompile: " <> sshow (Pact4._cmdHash tx)
  case payload of
    Exec (ExecMsg parsedCode _) ->
      case compileCode parsedCode of
        Left perr -> throwError $ InsertErrorCompilationFailed (sshow perr)
        Right _ -> return tx
    _ -> return tx
  where
    payload = Pact4._pPayload $ Pact4.payloadObj $ Pact4._cmdPayload tx
    compileCode p =
      let e = ParseEnv (chainweb216Pact v cid bh)
      in compileExps e (mkTextInfo (Pact4._pcCode p)) (Pact4._pcExps p)

checkParse
  :: (Logger logger)
  => logger
  -> ChainwebVersion
  -> ChainId
  -> BlockHeight
  -> Pact4.Command (Pact4.PayloadWithText Pact4.PublicMeta Text)
  -> ExceptT InsertError IO Pact4.Transaction
checkParse logger v cid bh tx = do
  liftIO $ logFunctionText logger Debug $ "Pact4.checkParse: " <> sshow (Pact4._cmdHash tx)
  forMOf (traversed . traversed) tx
    (either (throwError . InsertErrorPactParseError . T.pack) return . Pact4.parsePact (pact4ParserVersion v cid bh))

execTransactions
    :: (Logger logger)
    => Miner
    -> Vector Pact4.Transaction
    -> EnforceCoinbaseFailure
    -> CoinbaseUsePrecompiled
    -> Maybe Pact4.Gas
    -> Maybe Micros
    -> PactBlockM logger tbl (Transactions Pact4 (Either CommandInvalidError (Pact4.CommandResult [Pact4.TxLogJson])))
execTransactions miner ctxs enfCBFail usePrecomp gasLimit timeLimit = do
    mc <- initModuleCacheForBlock
    -- for legacy reasons (ask Emily) we don't use the module cache resulting
    -- from coinbase to run the pact cmds
    coinOut <- runCoinbase miner enfCBFail usePrecomp mc
    T2 txOuts _mcOut <- applyPactCmds ctxs miner mc gasLimit timeLimit
    return $! Transactions (V.zip ctxs txOuts) coinOut

initModuleCacheForBlock :: (Logger logger) => PactBlockM logger tbl ModuleCache
initModuleCacheForBlock = do
  PactServiceState{..} <- get
  isGenesis <- view psIsGenesis
  pbh <- views psParentHeader (view blockHeight . _parentHeader)
  case Map.lookupLE pbh _psInitCache of
    Nothing -> if isGenesis
      then return mempty
      else do
        mc <- Pact4.readInitModules
        updateInitCacheM mc
        return mc
    Just (_,mc) -> pure mc

runCoinbase
    :: (Logger logger)
    => Miner
    -> EnforceCoinbaseFailure
    -> CoinbaseUsePrecompiled
    -> ModuleCache
    -> PactBlockM logger tbl (Pact4.CommandResult [Pact4.TxLogJson])
runCoinbase miner enfCBFail usePrecomp mc = do
    isGenesis <- view psIsGenesis
    if isGenesis
    then return noCoinbase
    else do
      logger <- view (psServiceEnv . psLogger)
      v <- view chainwebVersion
      txCtx <- getTxContext miner Pact4.noPublicMeta

      let !bh = ctxCurrentBlockHeight txCtx

      let reward = minerReward v bh
      dbEnv <- view psBlockDbEnv
      let pactDb = _cpPactDbEnv dbEnv

      T2 cr upgradedCacheM <-
          liftIO $ Pact4.applyCoinbase v logger pactDb reward txCtx enfCBFail usePrecomp mc
      mapM_ upgradeInitCache upgradedCacheM
      liftPactServiceM $ debugResult "runPact4Coinbase" (Pact4.crLogs %~ fmap J.Array $ cr)
      return $! cr

  where
    upgradeInitCache newCache = do
      liftPactServiceM $ logInfoPact "Updating init cache for upgrade"
      updateInitCacheM newCache


data CommandInvalidError
  = CommandInvalidGasPurchaseFailure !Pact4GasPurchaseFailure
  | CommandInvalidTxTimeout !TxTimeout

-- | Apply multiple Pact commands, incrementing the transaction Id for each.
-- The output vector is in the same order as the input (i.e. you can zip it
-- with the inputs.)
applyPactCmds
    :: forall logger tbl. (Logger logger)
    => Vector Pact4.Transaction
    -> Miner
    -> ModuleCache
    -> Maybe Pact4.Gas
    -> Maybe Micros
    -> PactBlockM logger tbl (T2 (Vector (Either CommandInvalidError (Pact4.CommandResult [Pact4.TxLogJson]))) ModuleCache)
applyPactCmds cmds miner startModuleCache blockGas txTimeLimit = do
    let txsGas txs = fromIntegral $ sumOf (traversed . _Right . to Pact4._crGas) txs
    (txOuts, T2 mcOut _) <- tracePactBlockM' "applyPactCmds" (\_ -> ()) (txsGas . fst) $
      flip runStateT (T2 startModuleCache blockGas) $
        go [] (zip [0..] $ V.toList cmds)
    return $! T2 (V.fromList . List.reverse $ txOuts) mcOut
  where
    go
      :: [Either CommandInvalidError (Pact4.CommandResult [Pact4.TxLogJson])]
      -> [(Word, Pact4.Transaction)]
      -> StateT
          (T2 ModuleCache (Maybe Pact4.Gas))
          (PactBlockM logger tbl)
          [Either CommandInvalidError (Pact4.CommandResult [Pact4.TxLogJson])]
    go !acc = \case
        [] -> do
            pure acc
        (txIdxInBlock, tx) : rest -> do
            r <- applyPactCmd (TxBlockIdx txIdxInBlock) miner txTimeLimit tx
            case r of
                Left e@(CommandInvalidTxTimeout _) -> do
                    pure (Left e : acc)
                Left e@(CommandInvalidGasPurchaseFailure _) -> do
                    go (Left e : acc) rest
                Right a -> do
                    go (Right a : acc) rest

applyPactCmd
  :: (Logger logger)
  => TxIdxInBlock
  -> Miner
  -> Maybe Micros
  -> Pact4.Transaction
  -> StateT
      (T2 ModuleCache (Maybe Pact4.Gas))
      (PactBlockM logger tbl)
      (Either CommandInvalidError (Pact4.CommandResult [Pact4.TxLogJson]))
applyPactCmd txIdxInBlock miner txTimeLimit cmd = StateT $ \(T2 mcache maybeBlockGasRemaining) -> do
  dbEnv <- view psBlockDbEnv
  let pactDb = _cpPactDbEnv dbEnv
  prevBlockState <- liftIO $ fmap _benvBlockState $
    readMVar $ pdPactDbVar pactDb
  logger <- view (psServiceEnv . psLogger)
  gasLogger <- view (psServiceEnv . psGasLogger)
  txFailuresCounter <- view (psServiceEnv . psTxFailuresCounter)
  isGenesis <- view psIsGenesis
  v <- view chainwebVersion
  let
    -- for errors so fatal that the tx doesn't make it in the block
    onFatalError e
      | Just (Pact4BuyGasFailure f) <- fromException e = pure (Left (CommandInvalidGasPurchaseFailure f), T2 mcache maybeBlockGasRemaining)
      | Just t@(TxTimeout {}) <- fromException e = do
        -- timeouts can occur at any point during the transaction, even after
        -- gas has been bought (or even while gas is being redeemed, after the
        -- transaction proper is done). therefore we need to revert the block
        -- state ourselves if it happens.
        liftIO $ Pact4.modifyMVar'
          (pdPactDbVar pactDb)
          (benvBlockState .~ prevBlockState)
        pure (Left (CommandInvalidTxTimeout t), T2 mcache maybeBlockGasRemaining)
      | otherwise = throwM e
    requestedTxGasLimit = view Pact4.cmdGasLimit (Pact4.payloadObj <$> cmd)
    -- notice that we add 1 to the remaining block gas here, to distinguish the
    -- cases "tx used exactly as much gas remained in the block" (which is fine)
    -- and "tx attempted to use more gas than remains in the block" (which is
    -- illegal). for example: tx has a tx gas limit of 10000. the block has 5000
    -- remaining gas. therefore the tx is applied with a tx gas limit of 5001.
    -- if it uses 5001, that's illegal; if it uses 5000 or less, that's legal.
    newTxGasLimit = case maybeBlockGasRemaining of
      Nothing -> requestedTxGasLimit
      Just blockGasRemaining -> min (fromIntegral blockGasRemaining) requestedTxGasLimit
    gasLimitedCmd =
      set Pact4.cmdGasLimit newTxGasLimit (Pact4.payloadObj <$> cmd)
    initialGas = Pact4.initialGasOf (Pact4._cmdPayload cmd)
  let !hsh = Pact4._cmdHash cmd

  handle onFatalError $ do
    T2 result mcache' <- do
      txCtx <- getTxContext miner (Pact4.publicMetaOf gasLimitedCmd)
      let gasModel = getGasModel txCtx
      if isGenesis
      then liftIO $! Pact4.applyGenesisCmd logger pactDb Pact4.noSPVSupport txCtx gasLimitedCmd
      else do
        bhdb <- view (psServiceEnv . psBlockHeaderDb)
        parent <- view psParentHeader
        let spv = Pact4.pactSPV bhdb (_parentHeader parent)
        let
          !timeoutError = TxTimeout (Pact4.requestKeyToTransactionHash $ Pact4.cmdToRequestKey cmd)
          txTimeout io = case txTimeLimit of
            Nothing -> do
              logFunctionText logger Debug $ "txTimeLimit was not set - defaulting to a function of the block gas limit"
              io
            Just limit -> do
              logFunctionText logger Debug $ "txTimeLimit was " <> sshow limit
              maybe (throwM timeoutError) return =<< newTimeout (fromIntegral limit) io
          txGas (T3 r _ _) = fromIntegral $ Pact4._crGas r
        T3 r c _warns <- do
          tracePactBlockM' "applyCmd" (\_ -> J.toJsonViaEncode hsh) txGas $ do
            liftIO $ txTimeout $
              Pact4.applyCmd v logger gasLogger txFailuresCounter pactDb miner gasModel txCtx txIdxInBlock spv gasLimitedCmd initialGas mcache ApplySend
        pure $ T2 r c

    if isGenesis
    then updateInitCacheM mcache'
    else liftPactServiceM $ debugResult "applyPactCmd" (Pact4.crLogs %~ fmap J.Array $ result)

    -- mark the tx as processed at the checkpointer.
    liftIO $ _cpRegisterProcessedTx dbEnv (coerce $ Pact4.toUntypedHash hsh)
    case maybeBlockGasRemaining of
      Just blockGasRemaining
        | Left _ <- Pact4._pactResult (Pact4._crResult result)
        , blockGasRemaining < fromIntegral requestedTxGasLimit
        -> throwM $ BlockGasLimitExceeded $ Pact5.Gas $ fromIntegral requestedTxGasLimit - fromIntegral blockGasRemaining
          -- ^ this tx attempted to consume more gas than remains in the
          -- block, so the block is invalid. we know this because failing
          -- transactions consume their entire gas limit.
      _ -> return ()
    let maybeBlockGasRemaining' = (\g -> g - Pact4._crGas result) <$> maybeBlockGasRemaining
    pure (Right result, T2 mcache' maybeBlockGasRemaining')

pact4TransactionsFromPayload
    :: Pact4.PactParserVersion
    -> PayloadData
    -> IO (Vector Pact4.Transaction)
pact4TransactionsFromPayload ppv plData = do
    vtrans <- fmap V.fromList $
              mapM toCWTransaction $
              toList (view payloadDataTransactions plData)
    let (theLefts, theRights) = partitionEithers $ V.toList vtrans
    unless (null theLefts) $ do
        let ls = map T.pack theLefts
        throwM $ TransactionDecodeFailure $ "Failed to decode pact transactions: "
            <> T.intercalate ". " ls
    return $! V.fromList theRights
  where
    toCWTransaction bs = evaluate (force (codecDecode (Pact4.payloadCodec ppv) $
                                          _transactionBytes bs))

debugResult :: J.Encode a => Logger logger => Text -> a -> PactServiceM logger tbl ()
debugResult msg result =
  logDebugPact $ trunc $ msg <> " result: " <> J.encodeText result
  where
    trunc t | T.length t < limit = t
            | otherwise = T.take limit t <> " [truncated]"
    limit = 5000


-- | Calculate miner reward.
--
-- See: 'rewards/miner_rewards.csv'
--
minerReward
    :: ChainwebVersion
    -> BlockHeight
    -> Pact4.ParsedDecimal
minerReward v = Pact4.ParsedDecimal
    . _kda
    . minerRewardKda
    . blockMinerReward v
{-# INLINE minerReward #-}

data CRLogPair = CRLogPair Pact4.Hash [Pact4.TxLogJson]

instance J.Encode CRLogPair where
  build (CRLogPair h logs) = J.object
    [ "hash" J..= h
    , "rawLogs" J..= J.Array logs
    ]
  {-# INLINE build #-}

validateHashes
    :: BlockHeader
        -- ^ Current Header
    -> CheckablePayload
    -> Miner
    -> Transactions Pact4 (Pact4.CommandResult [Pact4.TxLogJson])
    -> Either PactException PayloadWithOutputs
validateHashes bHeader payload miner transactions =
    if newHash == prevHash
      then Right actualPwo
      else Left $ BlockValidationFailure $ BlockValidationFailureMsg $
              J.encodeText $ J.object
                [ "header" J..= J.encodeWithAeson (ObjectEncoded bHeader)
                , "actual" J..= J.encodeWithAeson actualPwo
                , "expected" J..?= case payload of
                    CheckablePayload _ -> Nothing
                    CheckablePayloadWithOutputs pwo -> Just $ J.encodeWithAeson pwo
                ]
  where

    actualPwo = toPayloadWithOutputs Pact4T miner transactions

    newHash = _payloadWithOutputsPayloadHash actualPwo
    prevHash = view blockPayloadHash bHeader


-- | This timeout variant returns Nothing if the timeout elapsed, regardless of whether or not it was actually able to interrupt its argument.
--   This is more robust in the face of scheduler behavior than the standard 'System.Timeout.timeout', with small timeouts.
newTimeout :: Int -> IO a -> IO (Maybe a)
newTimeout n f = do
  (timeSpan, a) <- Chronos.stopwatch (timeout n f)
  if Chronos.getTimespan timeSpan > fromIntegral (n * 1000)
  then return Nothing
  else return a
