{-# LANGUAGE TemplateHaskell #-}

module Sol.P0042 (compute, solve) where

import Data.Char (ord)

import qualified Data.ByteString.Char8 as BS (ByteString, unpack)
import qualified Data.FileEmbed as FE (embedFile, makeRelativeToProject)

import Mylib.Math (isTriangular)
import Mylib.Util (wordsWhen)

fileData :: BS.ByteString
fileData = $(FE.makeRelativeToProject "resources/0042_words.txt" >>= FE.embedFile)

parseData :: String -> [String]
parseData = wordsWhen (== ',') . filter (/= '"')

compute :: String
compute =
    show
        . length
        . filter isTriangular
        $ score <$> parseData (BS.unpack fileData)
  where
    score :: [Char] -> Int
    score = sum . map (\c -> ord c - ord 'A' + 1)

solve :: String
solve = compute
