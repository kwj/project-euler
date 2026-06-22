{-# LANGUAGE TemplateHaskell #-}

module Sol.P0022 (compute, solve) where

import Data.Char (ord)
import Data.List (sort)

import qualified Data.ByteString.Char8 as BS (ByteString, filter, foldl, split)
import qualified Data.FileEmbed as FE (embedFile, makeRelativeToProject)

fileData :: BS.ByteString
fileData = $(FE.makeRelativeToProject "resources/0022_names.txt" >>= FE.embedFile)

parseData :: BS.ByteString -> [BS.ByteString]
parseData = BS.split ',' . BS.filter (/= '"')

compute :: String
compute =
    show . sum . zipWith score [1 ..] . sort $ parseData fileData
  where
    score :: Int -> BS.ByteString -> Int
    score idx = (idx *) . BS.foldl (\acc c -> acc + ord c - ord 'A' + 1) 0

solve :: String
solve = compute
