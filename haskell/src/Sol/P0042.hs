{-# LANGUAGE TemplateHaskell #-}

module Sol.P0042 (compute, solve) where

import Data.Char (ord)

import qualified Data.ByteString.Char8 as BS (ByteString, unpack)
import qualified Data.FileEmbed as FE (embedFile, makeRelativeToProject)

import Mylib.Math (isTriangular)

fileData :: BS.ByteString
fileData = $(FE.makeRelativeToProject "resources/0042_words.txt" >>= FE.embedFile)

parseData :: String -> [String]
parseData =
    wordsWhen (== ',') . filter (/= '"')
  where
    wordsWhen :: (Char -> Bool) -> String -> [String]
    wordsWhen p s =
        aux (break p s) []
      where
        aux :: (String, String) -> [String] -> [String]
        aux ("", "") acc = reverse acc
        aux ("", tl) acc = aux (break p (drop 1 tl)) acc
        aux (hd, tl) acc = aux (break p (drop 1 tl)) (hd : acc)

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
