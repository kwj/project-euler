{-# LANGUAGE TemplateHaskell #-}

module Sol.P0059 (compute, solve) where

import Data.Bits (xor)
import Data.Char (chr, isAlphaNum, isPrint, isSpace, ord)
import Data.Function (on)
import Data.List (maximumBy)

import qualified Data.ByteString.Char8 as BS (ByteString, unpack)
import qualified Data.FileEmbed as FE (embedFile, makeRelativeToProject)

import Mylib.Combinatorics (cartesianProduct)
import Mylib.Util (wordsWhen)

fileData :: BS.ByteString
fileData = $(FE.makeRelativeToProject "resources/0059_cipher.txt" >>= FE.embedFile)

parseData :: String -> [Int]
parseData = map (read :: String -> Int) . wordsWhen (== ',')

score :: [Char] -> Int
score =
    sum . map score'
  where
    score' :: Char -> Int
    score' x
        | isAlphaNum x = 3
        | isSpace x = 2
        | otherwise = 1

decodeData :: [Int] -> [Int] -> [Char]
decodeData key = zipWith (\a b -> chr $ xor a b) (cycle key)

compute :: String
compute =
    show
        . foldl (\acc c -> acc + ord c) 0
        . maximumBy (compare `on` score)
        . filter (all isValidChar) -- pruning
        $ map
            (`decodeData` parseData (BS.unpack fileData))
            (cartesianProduct $ replicate 3 [ord 'a' .. ord 'z'])
  where
    isValidChar :: Char -> Bool
    isValidChar x = any ($ x) [isPrint, isSpace]

solve :: String
solve = compute
