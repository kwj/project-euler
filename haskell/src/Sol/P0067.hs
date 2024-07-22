{-# LANGUAGE TemplateHaskell #-}

module Sol.P0067 (compute, solve) where

import qualified Data.ByteString.Char8 as BS (ByteString, unpack)
import qualified Data.FileEmbed as FE (embedFile, makeRelativeToProject)

import Mylib.Util (headExn, tailExn)

fileData :: BS.ByteString
fileData = $(FE.makeRelativeToProject "resources/0067_triangle.txt" >>= FE.embedFile)

parseData :: String -> [[Int]]
parseData =
    (map . map) (read :: String -> Int) . map words . lines

compute :: String
compute =
    show
        . headExn
        $ foldr1
            (\x acc -> zipWith (+) x (zipWith max acc (tailExn acc)))
            (parseData (BS.unpack fileData))

solve :: String
solve = compute
