{-# LANGUAGE TemplateHaskell #-}

module Sol.P0099 (compute, solve) where

import Data.Function (on)
import Data.List (maximumBy)

import qualified Data.ByteString.Char8 as BS (ByteString, unpack)
import qualified Data.FileEmbed as FE (embedFile, makeRelativeToProject)

import Mylib.Util (wordsWhen)

fileData :: BS.ByteString
fileData = $(FE.makeRelativeToProject "resources/0099_base_exp.txt" >>= FE.embedFile)

parseData :: String -> [[Double]]
parseData = map (map (read :: String -> Double) . wordsWhen (== ',')) . lines

compute :: String
compute =
    show
        . fst
        . maximumBy (compare `on` snd)
        . zip [1 :: Int ..]
        $ (\lst -> (lst !! 1) * (log (lst !! 0))) <$> parseData (BS.unpack fileData)

solve :: String
solve = compute
