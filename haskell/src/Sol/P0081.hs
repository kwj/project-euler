{-# LANGUAGE TemplateHaskell #-}

module Sol.P0081 (compute, solve) where

import Data.List (mapAccumL)

import qualified Data.ByteString.Char8 as BS (ByteString, unpack)
import qualified Data.FileEmbed as FE (embedFile, makeRelativeToProject)

import Mylib.Util (headExn, lastExn)

fileData :: BS.ByteString
fileData = $(FE.makeRelativeToProject "resources/0081_matrix.txt" >>= FE.embedFile)

parseData :: String -> [[Int]]
parseData =
    map (map (read :: String -> Int) . wordsWhen (\c -> c == ',')) . lines
  where
    wordsWhen :: (Char -> Bool) -> String -> [String]
    wordsWhen p s =
        aux (break p s) []
      where
        aux ("", "") acc = reverse acc
        aux ("", tl) acc = aux (break p (drop 1 tl)) acc
        aux (hd, tl) acc = aux (break p (drop 1 tl)) (hd : acc)

compute :: String
compute =
    show
        . lastExn
        $ foldl
            auxRightward
            (scanl1 (+) (headExn matrix))
            (drop 1 matrix)
  where
    auxRightward :: [Int] -> [Int] -> [Int]
    auxRightward prev crnt =
        let (x, xs) =
                mapAccumL
                    (\a tpl -> (snd tpl + (min a (fst tpl)), a))
                    maxBound
                    (zipWith (\a b -> (a, b)) prev crnt)
         in (drop 1 xs) ++ [x]
    matrix = parseData (BS.unpack fileData)

solve :: String
solve = compute
