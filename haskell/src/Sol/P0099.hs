{-# LANGUAGE TemplateHaskell #-}

module Sol.P0099 (compute, solve) where

import Data.Function (on)
import Data.List (maximumBy, unfoldr)

import qualified Data.ByteString.Char8 as BS (
    ByteString,
    dropWhile,
    lines,
    readInt,
 )
import qualified Data.FileEmbed as FE (embedFile, makeRelativeToProject)

fileData :: BS.ByteString
fileData = $(FE.makeRelativeToProject "resources/0099_base_exp.txt" >>= FE.embedFile)

{- HLINT ignore parseData "Use head" -}
parseData :: BS.ByteString -> [(Double, Double)]
parseData =
    map
        ( (\lst -> (fromIntegral $ lst !! 0, fromIntegral $ lst !! 1))
            . unfoldr (BS.readInt . BS.dropWhile (== ','))
        )
        <$> BS.lines

compute :: String
compute =
    show
        . fst
        . maximumBy (compare `on` snd)
        . zip [1 :: Int ..]
        $ (\(b, e) -> e * log b) <$> parseData fileData

solve :: String
solve = compute
