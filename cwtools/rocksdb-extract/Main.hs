{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where


import Control.Exception hiding (Handler)
import Chainweb.BlockHash
import Chainweb.BlockHeader (blockHeight, blockForkNumber, blockHash, blockPayloadHash)
import Chainweb.BlockHeaderDB.Internal (BlockHeaderDb(..), RankedBlockHeader(..), unsafeInsertBlockHeaderDb)
import Chainweb.BlockHeight (BlockHeight(..))
import Chainweb.ChainId
import Chainweb.TreeDB
import Chainweb.Cut.CutHashes (cutIdToText)
import Chainweb.CutDB (cutHashesTable)
import Chainweb.Logger (Logger, l2l, setComponent, logFunctionText)
import Chainweb.Pact.Backend.ChainwebPactDb ()
import Chainweb.Pact.Backend.PactState
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Backend.Utils
import Chainweb.Pact.Payload.PayloadStore (addNewPayload, lookupPayloadWithHeight)
import Chainweb.Pact.Payload.PayloadStore.RocksDB (newPayloadDb)
import Chainweb.Storage.Table (Iterator(..), Entry(..), withTableIterator, unCasify, tableInsert)
import Chainweb.Storage.Table.RocksDB (RocksDb, withRocksDb, withReadOnlyRocksDb, modernDefaultOptions, compactRangeRocksDb)
import Chainweb.Utils (sshow, fromTextM, toText, int)
import Chainweb.Version (ChainId, HasVersion(..), withVersion, ChainwebVersion(..))
import Chainweb.Version.Guards (minimumBlockHeaderHistory)
import Chainweb.Version.Mainnet (mainnet)
import Chainweb.Version.Registry (findKnownVersion)
import Chainweb.Version.Testnet04 (testnet04)
import Chainweb.WebBlockHeaderDB (getWebBlockHeaderDb, initWebBlockHeaderDb)

import qualified Streaming.Prelude as S
import Control.Lens
--import Control.Monad (forM, forM_, unless, void, when)
--import Control.Monad.IO.Class (MonadIO(liftIO))
--import Control.Monad.Trans.Control (MonadBaseControl, liftBaseOp)
--import Control.Monad.Trans.Resource (runResourceT)
--import Data.Function ((&))
--import Data.Int (Int64)
--import Data.LogMessage (SomeLogMessage, logText)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Bool
import qualified Data.HashSet as HS
--import Database.RocksDB.Types (Options(..), Compression(..))
--import Database.SQLite3 qualified as Lite
--import Database.SQLite3.Direct (Utf8(..), Database)
import Options.Applicative qualified as O
--import Prelude hiding (log)
-- import Streaming qualified as S
-- import Streaming.Prelude qualified as S
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
--import System.Exit (exitFailure)
--import System.FilePath ((</>))
--import System.IO (Handle)
--import System.IO qualified as IO
--import UnliftIO.Async (pooledForConcurrently_)


data Config = Config
  { chainwebVersion :: ChainwebVersion
  , fromDir :: FilePath
  , toDir :: FilePath
  , minHeight:: Maybe BlockHeight
  , maxHeight :: Maybe BlockHeight
  , cid :: ChainId
  }
  deriving (Show)


getConfig :: IO Config
getConfig = do
  O.execParser opts
  where
    opts :: O.ParserInfo Config
    opts = O.info (parser O.<**> O.helper) (O.fullDesc <> O.progDesc "Payload Db extracting tool: Extract payloads from a global datatabse")

    parser :: O.Parser Config
    parser = do
      chainwebVersion <- parseVersion <$> O.strOption (O.long "chainweb-version" <> O.value "mainnet01")
      fromDir <- O.strOption (O.long "from" <> O.help "Directory containing source RocksDB}")
      toDir <- O.strOption (O.long "to" <> O.help "Directory where to place the target RocksDB")
      minHeight <- (fmap parseHeight) <$> O.optional (O.strOption (O.long "min-height"))
      maxHeight <- (fmap parseHeight) <$> O.optional ( O.strOption (O.long "max-height"))

      cid <- parseChainId <$> O.strOption (O.long "chain")

      return Config {..}

    parseHeight :: String -> BlockHeight
    parseHeight = BlockHeight . fromIntegral . read @Int

    parseChainId :: String -> ChainId
    parseChainId = read

    parseVersion :: Text -> ChainwebVersion
    parseVersion =
      fromMaybe (error "ChainwebVersion parse failed")
      . (>>= findKnownVersion)
      . fromTextM

main :: IO ()
main = do
  cfg <- getConfig
  checkDirs cfg
  print cfg
  doCopy cfg

checkDirs :: Config -> IO ()
checkDirs Config{..} =    doesDirectoryExist fromDir >>= bool (error "Src Dir does not exist") (pure ())
                       >> doesDirectoryExist toDir >>= bool (pure ()) (error "Dest dir already exists")
                       >> createDirectoryIfMissing True toDir

doCopy :: Config -> IO()
doCopy Config{..} = do
  withVersion chainwebVersion $
    withReadOnlyRocksDb fromDir modernDefaultOptions $ \srcDb -> do
      withRocksDb toDir modernDefaultOptions $ \targetDb -> do
        let srcPayloads = newPayloadDb srcDb
        let targetPayloads = newPayloadDb targetDb

        srcWbhdb <- initWebBlockHeaderDb srcDb
        targetWbhdb <- initWebBlockHeaderDb targetDb
        srcHdb <- getWebBlockHeaderDb srcWbhdb cid
        dstHdb <- getWebBlockHeaderDb targetWbhdb cid

        tip <- maxEntry srcHdb
        genesisRank <- minRank srcHdb

        getBranchIncreasing srcHdb tip genesisRank $ \headers ->
          headers
          & (case maxHeight of
            Nothing     -> id
            Just height -> S.takeWhile (\h -> (h ^. blockHeight) <= height))
          &  S.mapM_ (\h -> do
                lookupPayloadWithHeight srcPayloads (Just $ h ^. blockHeight) (h ^. blockPayloadHash) >>= \case
                    Nothing -> error "Missing payload: This is likely due to a corrupted database."
                    Just payloadWithOutputs -> addNewPayload targetPayloads (h ^. blockHeight) payloadWithOutputs

                unsafeInsertBlockHeaderDb dstHdb h)
