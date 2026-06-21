{-# LANGUAGE OverloadedStrings #-}
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
        (\s (from, to) -> T.replace from to s)
        subsutitutionTbl
  where
    subsutitutionTbl :: [(T.Text, T.Text)]
    subsutitutionTbl =
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
    show . foldl (\acc s -> acc + T.length s - T.length (replaceNumStr s)) 0 $
        parseData (BS.unpack fileData)

solve :: String
solve = compute
