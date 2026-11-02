{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Chainweb.Pact.Payload.PayloadStore.RocksDB
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Content addressable block payload store that uses RocksDB as storage backend.
--
module Chainweb.Pact.Payload.PayloadStore.RocksDB
( newTransactionDb
, newPayloadDb
) where

-- internal modules

import Chainweb.BlockHeight
import Chainweb.Pact.Payload
import Chainweb.Pact.Payload.PayloadStore
import Chainweb.Utils hiding (Codec)
import Chainweb.Utils.Serialization

import Chainweb.Storage.Table
import Chainweb.Storage.Table.RocksDB

-- -------------------------------------------------------------------------- --
-- RocksDbCas

newTransactionDb :: RocksDb -> TransactionDb RocksDbTable
newTransactionDb db = TransactionDb
    newBlockPayloadHeightsStore
    newBlockPayloadStore
    newBlockTransactionsStore
    oldBlockPayloadStore
    oldBlockTransactionsStore
  where
    newBlockPayloadHeightsStore :: RocksDbTable BlockPayloadHash BlockHeight
    newBlockPayloadHeightsStore = newTable db
        (Codec (runPutS . encodeBlockHeight) (runGetS decodeBlockHeight) True)
        (Codec (runPutS . encodeBlockPayloadHash) (runGetS decodeBlockPayloadHash) True)
        ["BlockPayloadIndex"]

    newBlockPayloadStore :: RocksDbTable (BlockHeight, BlockPayloadHash) BlockPayload
    newBlockPayloadStore = newTable db
        (Codec encodeBlockPayloads decodeBlockPayloads True)
        (Codec
          (\(bh, bp) -> runPutS (encodeBlockHeight bh >> encodeBlockPayloadHash bp))
          (runGetS ((,) <$> decodeBlockHeight <*> decodeBlockPayloadHash)) True)
        ["BlockPayload2"]

    newBlockTransactionsStore :: RocksDbTable (BlockHeight, BlockTransactionsHash) BlockTransactions
    newBlockTransactionsStore = newTable db
        (Codec encodeBlockTransactions decodeBlockTransactions True)
        (Codec
          (\(h, hsh) -> runPutS $ encodeBlockHeight h >> encodeBlockTransactionsHash hsh)
          (runGetS ((,) <$> decodeBlockHeight <*> decodeBlockTransactionsHash)) True)
        ["BlockTransactions2"]

    oldBlockPayloadStore :: Casify RocksDbTable BlockPayload
    oldBlockPayloadStore = Casify $ newTable db
        (Codec encodeToByteString decodeStrictOrThrow' True)
        (Codec (runPutS . encodeBlockPayloadHash) (runGetS decodeBlockPayloadHash) True)
        ["BlockPayload"]

    oldBlockTransactionsStore :: Casify RocksDbTable BlockTransactions
    oldBlockTransactionsStore = Casify $ newTable db
        (Codec encodeToByteString decodeStrictOrThrow' True)
        (Codec (runPutS . encodeBlockTransactionsHash) (runGetS decodeBlockTransactionsHash) True)
        ["BlockTransactions"]

newPayloadDb :: RocksDb -> PayloadDb RocksDbTable
newPayloadDb db = PayloadDb (newTransactionDb db) newPayloadCache
  where
    newPayloadCache :: PayloadCache RocksDbTable
    newPayloadCache = PayloadCache blockOutputsStore transactionTreeStore outputTreeStore

    blockOutputsStore :: BlockOutputsStore RocksDbTable
    blockOutputsStore = BlockOutputsStore oldBlockOutputsTbl newBlockOutputsTbl
      where
        oldBlockOutputsTbl :: Casify RocksDbTable BlockOutputs
        oldBlockOutputsTbl = Casify $ newTable db
            (Codec encodeToByteString decodeStrictOrThrow' True)
            (Codec (runPutS . encodeBlockOutputsHash) (runGetS decodeBlockOutputsHash) True)
            ["BlockOutputs"]

        newBlockOutputsTbl :: RocksDbTable (BlockHeight, BlockOutputsHash) BlockOutputs
        newBlockOutputsTbl = newTable db
            (Codec encodeBlockOutputs decodeBlockOutputs True)
            (Codec
                (\(h, hsh) -> runPutS $ encodeBlockHeight h >> encodeBlockOutputsHash hsh)
                (runGetS $ (,) <$> decodeBlockHeight <*> decodeBlockOutputsHash) True)
            ["BlockOutputs2"]

    transactionTreeStore :: TransactionTreeStore RocksDbTable
    transactionTreeStore = TransactionTreeStore oldTransactionTreeTbl newTransactionTreeTbl
      where
        oldTransactionTreeTbl :: Casify RocksDbTable TransactionTree
        oldTransactionTreeTbl = Casify $ newTable db
            (Codec encodeToByteString decodeStrictOrThrow' True)
            (Codec (runPutS . encodeBlockTransactionsHash) (runGetS decodeBlockTransactionsHash) True)
            ["TransactionTree"]

        newTransactionTreeTbl :: RocksDbTable (BlockHeight, BlockTransactionsHash) TransactionTree
        newTransactionTreeTbl = newTable db
            (Codec encodeTransactionTree decodeTransactionTree True)
            (Codec
                (\(h, hsh) -> runPutS $ encodeBlockHeight h >> encodeBlockTransactionsHash hsh)
                (runGetS $ (,) <$> decodeBlockHeight <*> decodeBlockTransactionsHash) True)
            ["TransactionTree2"]

    outputTreeStore :: OutputTreeStore RocksDbTable
    outputTreeStore = OutputTreeStore oldOutputTreeTbl newOutputTreeTbl
      where
        oldOutputTreeTbl :: Casify RocksDbTable OutputTree
        oldOutputTreeTbl = Casify $ newTable db
            (Codec encodeToByteString decodeStrictOrThrow' True)
            (Codec (runPutS . encodeBlockOutputsHash) (runGetS decodeBlockOutputsHash) True)
            ["OutputTree"]

        newOutputTreeTbl :: RocksDbTable (BlockHeight, BlockOutputsHash) OutputTree
        newOutputTreeTbl = newTable db
            (Codec encodeOutputTree decodeOutputTree True)
            (Codec
                (\(h, hsh) -> runPutS $ encodeBlockHeight h >> encodeBlockOutputsHash hsh)
                (runGetS $ (,) <$> decodeBlockHeight <*> decodeBlockOutputsHash) True)
            ["OutputTree2"]
