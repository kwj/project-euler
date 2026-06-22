{-# LANGUAGE TemplateHaskell #-}

module Sol.P0042 (compute, solve) where

import Data.Char (ord)

import qualified Data.ByteString.Char8 as BS (ByteString, filter, foldl, split)
import qualified Data.FileEmbed as FE (embedFile, makeRelativeToProject)

import Mylib.Math (isTriangular)

fileData :: BS.ByteString
fileData = $(FE.makeRelativeToProject "resources/0042_words.txt" >>= FE.embedFile)

parseData :: BS.ByteString -> [BS.ByteString]
parseData = BS.split ',' . BS.filter (/= '"')

compute :: String
compute =
    show . length . filter isTriangular . map score $ parseData fileData
  where
    score :: BS.ByteString -> Int
    score = BS.foldl (\acc c -> acc + ord c - ord 'A' + 1) 0

solve :: String
solve = compute
