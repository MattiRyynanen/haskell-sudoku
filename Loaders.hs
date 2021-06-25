{-# LANGUAGE DeriveGeneric #-}

module Loaders where

import Definitions
import Data.Aeson
import Data.Text
import GHC.Generics
import qualified Data.ByteString.Lazy as B

data NamedPuzzle = NamedPuzzle { id :: Text, puzzleStr :: Text }
    deriving (Show, Generic)

instance FromJSON NamedPuzzle

jsonFile :: FilePath
jsonFile = "puzzles.json"

getJson :: IO B.ByteString
getJson = B.readFile jsonFile

--getPuzzles = getJson jsonFile

readPuzzlesFrom jsonFile = do
    -- Get JSON data and decode it
    d <- (eitherDecode <$> B.readFile jsonFile) :: IO (Either String [NamedPuzzle])
    -- If d is Left, the JSON was malformed.
    -- In that case, we report the error.
    -- Otherwise, we perform the operation of
    -- our choice. In this case, just print it.
    case d of
        Left err -> putStrLn err
        Right ps -> print ps