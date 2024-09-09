{-# LANGUAGE TemplateHaskell #-}

module Sol.P0022 (compute, solve) where

import Data.Char (ord)
import Data.List (sort)

import qualified Data.ByteString.Char8 as BS (ByteString, unpack)
import qualified Data.FileEmbed as FE (embedFile, makeRelativeToProject)

import Mylib.Util (wordsWhen)

fileData :: BS.ByteString
fileData = $(FE.makeRelativeToProject "resources/0022_names.txt" >>= FE.embedFile)

parseData :: String -> [String]
parseData = wordsWhen (== ',') . filter (/= '"')

compute :: String
compute =
    show
        . sum
        $ zipWith
            score
            [1 ..]
            (sort $ parseData (BS.unpack fileData))
  where
    score :: Int -> [Char] -> Int
    score i s = i * sum ((\c -> ord c - ord 'A' + 1) <$> s)

solve :: String
solve = compute
