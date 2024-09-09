{-# LANGUAGE TemplateHaskell #-}

module Sol.P0081 (compute, solve) where

import Data.List (mapAccumL)

import qualified Data.ByteString.Char8 as BS (ByteString, unpack)
import qualified Data.FileEmbed as FE (embedFile, makeRelativeToProject)

import Mylib.Util (headExn, lastExn, wordsWhen)

fileData :: BS.ByteString
fileData = $(FE.makeRelativeToProject "resources/0081_matrix.txt" >>= FE.embedFile)

parseData :: String -> [[Int]]
parseData = map (map (read :: String -> Int) . wordsWhen (== ',')) . lines

compute :: String
compute =
    show
        . lastExn
        $ foldl
            auxRightward
            (scanl1 (+) (headExn matrix))
            (drop 1 matrix)
  where
    matrix = parseData (BS.unpack fileData)

    auxRightward :: [Int] -> [Int] -> [Int]
    auxRightward prev crnt =
        drop 1 xs ++ [x]
      where
        (x, xs) =
            mapAccumL
                (\a tpl -> (snd tpl + min a (fst tpl), a))
                maxBound
                (zip prev crnt)

solve :: String
solve = compute
