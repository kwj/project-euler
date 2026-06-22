{-# LANGUAGE TemplateHaskell #-}

module Sol.P0059 (compute, solve) where

import Data.Bits (xor)
import Data.Char (chr, isAlphaNum, isPrint, isSpace, ord)
import Data.Function (on)
import Data.List (maximumBy, unfoldr)

import qualified Data.ByteString.Char8 as BS (ByteString, dropWhile, readInt)
import qualified Data.FileEmbed as FE (embedFile, makeRelativeToProject)

import Mylib.Combinatorics (cartesianProduct)

fileData :: BS.ByteString
fileData = $(FE.makeRelativeToProject "resources/0059_cipher.txt" >>= FE.embedFile)

parseData :: BS.ByteString -> [Int]
parseData = unfoldr (BS.readInt . BS.dropWhile (== ','))

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
        . map (`decodeData` parseData fileData)
        . cartesianProduct
        $ replicate 3 [ord 'a' .. ord 'z']
  where
    isValidChar :: Char -> Bool
    isValidChar x = any ($ x) [isPrint, isSpace]

solve :: String
solve = compute
