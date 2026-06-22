{-# LANGUAGE TemplateHaskell #-}

module Sol.P0067 (compute, solve) where

import Data.Char (isSpace)
import Data.List (unfoldr)

import qualified Data.ByteString.Char8 as BS (
    ByteString,
    dropWhile,
    lines,
    readInt,
 )
import qualified Data.FileEmbed as FE (embedFile, makeRelativeToProject)

import Mylib.Util (headExn, tailExn)

fileData :: BS.ByteString
fileData = $(FE.makeRelativeToProject "resources/0067_triangle.txt" >>= FE.embedFile)

parseData :: BS.ByteString -> [[Int]]
parseData =
    map (unfoldr (BS.readInt . BS.dropWhile isSpace)) <$> BS.lines

compute :: String
compute =
    show
        . headExn
        . foldr1 (\x acc -> zipWith (+) x (zipWith max acc (tailExn acc)))
        $ parseData fileData

solve :: String
solve = compute
