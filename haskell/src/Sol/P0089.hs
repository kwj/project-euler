{-# LANGUAGE TemplateHaskell #-}

module Sol.P0089 (compute, solve) where

import qualified Data.ByteString.Char8 as BS (ByteString, unpack)
import qualified Data.FileEmbed as FE (embedFile, makeRelativeToProject)
import qualified Data.Text as T (Text, length, pack, replace)

fileData :: BS.ByteString
fileData = $(FE.makeRelativeToProject "resources/0089_roman.txt" >>= FE.embedFile)

parseData :: String -> [T.Text]
parseData = map T.pack . lines

replaceNumStr :: T.Text -> T.Text
replaceNumStr =
    (flip . foldl)
        (\acc (from, to) -> T.replace (T.pack from) (T.pack to) acc)
        [ ("IIIIIIIII", "##")
        , ("XXXXXXXXX", "##")
        , ("CCCCCCCCC", "##")
        , ("VIIII", "##")
        , ("LXXXX", "##")
        , ("DCCCC", "##")
        , ("IIIII", "#")
        , ("XXXXX", "#")
        , ("CCCCC", "#")
        , ("IIII", "##")
        , ("XXXX", "##")
        , ("CCCC", "##")
        ]

compute :: String
compute =
    show
        . sum
        $ (\s -> T.length s - T.length (replaceNumStr s)) <$> parseData (BS.unpack fileData)

solve :: String
solve = compute
