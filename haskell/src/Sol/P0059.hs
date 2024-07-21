{-# LANGUAGE TemplateHaskell #-}

module Sol.P0059 (compute, solve) where

import Data.Bits (xor)
import Data.Char (ord)
import Data.Function (on)
import Data.List (maximumBy)

import qualified Data.ByteString.Char8 as BS (ByteString, unpack)
import qualified Data.FileEmbed as FE (embedFile, makeRelativeToProject)

import Mylib.Combinatorics (cartesianProduct)

fileData :: BS.ByteString
fileData = $(FE.makeRelativeToProject "resources/0059_cipher.txt" >>= FE.embedFile)

parseData :: String -> [Int]
parseData =
    map (read :: String -> Int) . wordsWhen (\c -> c == ',')
  where
    wordsWhen :: (Char -> Bool) -> String -> [String]
    wordsWhen p s =
        aux (break p s) []
      where
        aux ("", "") acc = reverse acc
        aux ("", tl) acc = aux (break p (drop 1 tl)) acc
        aux (hd, tl) acc = aux (break p (drop 1 tl)) (hd : acc)

score :: [Int] -> Int
score =
    sum . map score'
  where
    score' x
        | x == ord ' ' = 3 -- 0x20
        | x >= ord 'A' && x <= ord 'Z' = 5 -- 0x41 .. 0x5a
        | x >= ord 'a' && x <= ord 'z' = 3 -- 0x61 .. 0x7a
        | x >= ord '!' && x <= ord '~' = 1 -- 0x21 .. 0x7e
        | otherwise = 0

decodeData :: [Int] -> [Int] -> [Int]
decodeData key encData = zipWith xor (cycle key) encData

compute :: String
compute =
    show
        . sum
        . maximumBy (compare `on` score)
        $ zipWith
            (\key encData -> decodeData key encData)
            (cartesianProduct . take 3 $ repeat [ord 'a' .. ord 'z'])
            (repeat $ parseData (BS.unpack fileData))

solve :: String
solve = compute
