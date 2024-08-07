{-# LANGUAGE TemplateHaskell #-}

module Sol.P0098 (compute, solve) where

import Control.Monad (guard)
import Data.Array.IArray (Array, listArray, (!))
import Data.List (nub, singleton, sort)
import Data.Maybe (catMaybes)

import qualified Data.ByteString.Char8 as BS (ByteString, unpack)
import qualified Data.FileEmbed as FE (embedFile, makeRelativeToProject)

import Mylib.Util (digits, undigits, wordsWhen)

fileData :: BS.ByteString
fileData = $(FE.makeRelativeToProject "resources/0098_words.txt" >>= FE.embedFile)

parseData :: String -> [String]
parseData = wordsWhen (== ',') . filter (/= '"')

squareNumbers :: [Int]
squareNumbers = [n * n | n <- [1 ..]]

ndigitSquares :: Int -> [Int]
ndigitSquares n =
    takeWhile (< 10 ^ n) $ dropWhile (< 10 ^ (n - 1)) squareNumbers

findSquare :: String -> String -> [Int]
findSquare w1 w2 = do
    sq <- squares
    n <- makeNumber w1 w2 sq
    guard (elem n squares)
    [sq, n]
  where
    squares = ndigitSquares $ length w1

makeNumber :: String -> String -> Int -> [Int]
makeNumber w1 w2 sq
    | length trans_map /= (length $ nub w1) =
        []
    | length trans_map /= (length . nub $ snd <$> trans_map) =
        []
    | otherwise =
        singleton . undigits . reverse . catMaybes $ flip lookup trans_map <$> w2
  where
    trans_map = nub $ zip w1 (reverse $ digits sq)

compute :: String
compute =
    show . maximum $ do
        i <- [1 .. (len - 1)]
        j <- [(i + 1) .. len]
        guard $ isAnagram (word_array ! i) (word_array ! j)
        findSquare (word_array ! i) (word_array ! j)
  where
    keywords = parseData (BS.unpack fileData)
    len = length keywords
    word_array = listArray (1, len) keywords :: Array Int String

    isAnagram :: String -> String -> Bool
    isAnagram w1 w2 =
        length w1 == length w2 && sort w1 == sort w2

solve :: String
solve = compute
