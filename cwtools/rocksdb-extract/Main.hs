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


import Chainweb.BlockHash
import Chainweb.BlockHeader (BlockHeader, blockChainId, blockHeight, blockHash, blockPayloadHash)
import Chainweb.BlockHeaderDB.Internal (BlockHeaderDb(..), RankedBlockHeader(..))
import Chainweb.BlockHeight (BlockHeight(..))
import Chainweb.ChainId
import Chainweb.TreeDB
import Control.Monad (forM_)
import Chainweb.CutDB (cutHashesTable, readHighestCutHeaders)
import Chainweb.Pact.Backend.ChainwebPactDb ()
import Chainweb.Pact.Payload.PayloadStore (addNewPayload, lookupPayloadWithHeight)
import Chainweb.Pact.Payload.PayloadStore.RocksDB (newPayloadDb)
import Chainweb.Storage.Table (Iterator(..),  withTableIterator, tableInsert, casInsert)
import Chainweb.Storage.Table.RocksDB (withRocksDb, withReadOnlyRocksDb, modernDefaultOptions)
import Chainweb.Utils (fromJuste, fromTextM)
import Chainweb.Version (withVersion, ChainwebVersion(..))

import Chainweb.Version.Registry (findKnownVersion)
import Chainweb.WebBlockHeaderDB (getWebBlockHeaderDb, initWebBlockHeaderDb)

import qualified Streaming.Prelude as S
import Control.Lens

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Bool
import qualified Data.HashSet as HS
import Options.Applicative qualified as O
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)



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

insertBlockHeader:: BlockHeaderDb -> BlockHeader -> IO ()
insertBlockHeader db h = do
  let rbh = RankedBlockHeader  h
      rh = RankedBlockHash (h ^. blockHeight) (h ^. blockHash)
  tableInsert (_chainDbCas db) rh rbh
  tableInsert (_chainDbRankTable db) (h ^. blockHash) (h ^. blockHeight)


doCopy :: Config -> IO ()
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

        let srcCut = cutHashesTable srcDb
        let dstCut = cutHashesTable targetDb

        -- Retrieve and write the most recent Cut
        lastCut <- fromJuste <$> (withTableIterator srcCut $ (\i -> iterLast i >> iterValue i))
        casInsert dstCut lastCut

        -- And add all headers last headers related to that cut. Db is now consistent.
        tipCut <- readHighestCutHeaders (\_ x -> print $ Text.unpack x) srcWbhdb srcCut
        forM_ tipCut $  \h ->
          getWebBlockHeaderDb targetWbhdb (h ^. blockChainId) >>= flip insertBlockHeader h


        getBranch srcHdb HS.empty (HS.singleton (UpperBound (tipCut ^?! ix cid . blockHash)))
          -- For each header of the main branch
          & case minHeight of
              Nothing     -> id
              Just height -> S.takeWhile ((<) height . view blockHeight)

          & S.mapM (\h -> case (h ^. blockHeight) `mod` 100000 of
                            0 -> putStrLn ("Processing block: " <> show (h ^. blockHeight)) >> return h
                            _ -> return h)

          -- Copy inconditionnaly the header, and the associatted payload if present
          &  S.mapM_ (\h -> lookupPayloadWithHeight srcPayloads (Just $ h ^. blockHeight) (h ^. blockPayloadHash) >>= (\case
                                Nothing                 -> putStrLn ("Missing payload: This is likely due to a corrupted database." <> show (h ^. blockHeight))
                                Just payloadWithOutputs -> addNewPayload targetPayloads (h ^. blockHeight) payloadWithOutputs)
                            >> insertBlockHeader dstHdb h)
