{-# LANGUAGE TemplateHaskell #-}

module Sol.P0082 (compute, solve) where

import Data.List (mapAccumL, mapAccumR, transpose)

import qualified Data.ByteString.Char8 as BS (ByteString, unpack)
import qualified Data.FileEmbed as FE (embedFile, makeRelativeToProject)

import Mylib.Util (headExn, initExn, lastExn)

fileData :: BS.ByteString
fileData = $(FE.makeRelativeToProject "resources/0082_matrix.txt" >>= FE.embedFile)

parseData :: String -> [[Int]]
parseData =
    transpose
        . map (map (read :: String -> Int) . wordsWhen (\c -> c == ','))
        . lines
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
        . minimum
        $ foldl
            (\prev crnt -> auxLeftward crnt $ auxRightward prev crnt)
            (headExn matrix)
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
    auxLeftward :: [Int] -> [Int] -> [Int]
    auxLeftward crnt work =
        let (x, xs) =
                mapAccumR
                    (\a tpl -> (min (fst tpl + a) (snd tpl), a))
                    (lastExn work)
                    (zipWith (\a b -> (a, b)) crnt work)
         in x : (initExn xs)
    matrix = parseData (BS.unpack fileData)

solve :: String
solve = compute
