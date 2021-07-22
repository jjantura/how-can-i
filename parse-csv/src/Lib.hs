{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
  ( parseCSV,
  )
where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Conduit ()
import Data.Conduit.Binary ()
import Data.Conduit.List as CL ()
import Data.Csv (FromRecord, HasHeader (NoHeader), decode)
import Data.Text (Text)
import qualified Data.Vector as V
import GHC.Generics (Generic)
import System.IO ()

data Record = Record
  { v0 :: Text,
    v1 :: Text,
    v2 :: Text
  }
  deriving (Show, Eq, Read, Generic)

instance FromRecord Record

readData :: FilePath -> IO (Either String (V.Vector Record))
readData fp = do
  csvData <- BL.readFile "data.csv"
  return $ decode NoHeader csvData

extractRecord :: V.Vector Record -> Text -> Text -> Either Text Text
extractRecord v _v0 _v1 = if V.length matching == 1 then Right $ v2 $ V.head matching else Left "Fail"
  where
    matching = V.filter (\v -> v0 v == _v0 && v1 v == _v1) v -- Fix naming, it's confusing

parseCSV :: IO ()
parseCSV = do
  x <- readData "data.csv"
  case x of
    Left e -> putStrLn e
    Right r -> do
      let y = extractRecord r "r1c0" "r1c1"
      case y of
        Left l -> print l
        Right r -> print r