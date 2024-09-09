{-# LANGUAGE TemplateHaskell #-}

module Sol.P0079 (compute, solve) where

import Data.Map.Strict as M (Map, empty, insert, keys, lookup, member, (!))
import Data.Maybe (fromMaybe)
import Data.Set as S (Set, empty, insert)

import qualified Data.ByteString.Char8 as BS (ByteString, unpack)
import qualified Data.FileEmbed as FE (embedFile, makeRelativeToProject)

fileData :: BS.ByteString
fileData = $(FE.makeRelativeToProject "resources/0079_keylog.txt" >>= FE.embedFile)

dfs :: M.Map Char (S.Set Char) -> [Char] -> Char -> [Char]
dfs depInfo =
    visit []
  where
    visit :: [Char] -> [Char] -> Char -> [Char]
    visit temp visited node
        | node `elem` temp =
            error "cycle detection"
        | node `elem` visited =
            visited
        | M.member node depInfo =
            node : foldl (visit (node : temp)) visited (depInfo M.! node)
        | otherwise =
            [node]

parseData :: String -> M.Map Char (S.Set Char)
parseData =
    foldl aux M.empty . lines
  where
    aux :: M.Map Char (S.Set Char) -> [Char] -> M.Map Char (S.Set Char)
    aux depInfo [] = depInfo
    aux depInfo (x : xs) =
        aux
            (M.insert x (foldl (flip S.insert) set xs) depInfo)
            xs
      where
        set = fromMaybe S.empty (M.lookup x depInfo)

compute :: String
compute =
    foldl (dfs depInfo) [] (M.keys depInfo)
  where
    depInfo = parseData (BS.unpack fileData)

solve :: String
solve = compute
