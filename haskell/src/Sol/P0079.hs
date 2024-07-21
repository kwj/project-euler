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
dfs depInfo perm ch =
    visit [] perm ch
  where
    visit :: [Char] -> [Char] -> Char -> [Char]
    visit temp visited node
        | elem node temp == True =
            error "cycle detection"
        | elem node visited == True =
            visited
        | M.member node depInfo == True =
            node : (foldl (\acc x -> visit (node : temp) acc x) visited (depInfo M.! node))
        | otherwise =
            [node]

parseData :: String -> M.Map Char (S.Set Char)
parseData =
    foldl (\acc line -> aux acc line) M.empty . lines
  where
    aux :: M.Map Char (S.Set Char) -> [Char] -> M.Map Char (S.Set Char)
    aux depInfo [] = depInfo
    aux depInfo (x : xs) =
        aux
            (M.insert x (foldl (\acc n -> S.insert n acc) set xs) depInfo)
            xs
      where
        set = fromMaybe S.empty (M.lookup x depInfo)

compute :: String
compute =
    foldl (\acc x -> dfs depInfo acc x) [] (M.keys depInfo)
  where
    depInfo = parseData (BS.unpack fileData)

solve :: String
solve = compute
