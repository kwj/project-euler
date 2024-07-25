{-# LANGUAGE TemplateHaskell #-}

-- [TODO] This implementation is slow and needs to be improved.

module Sol.P0096 (compute, solve) where

import Control.Monad (guard)
import Data.Char (digitToInt)
import Data.Function (on)
import Data.List (find, minimumBy)
import Data.Maybe (fromJust)

import qualified Data.ByteString.Char8 as BS (ByteString, unpack)
import qualified Data.FileEmbed as FE (embedFile, makeRelativeToProject)

import Mylib.Util (headExn, partitionByStep, tailExn)

type Grid = [[Int]]

fileData :: BS.ByteString
fileData = $(FE.makeRelativeToProject "resources/0096_sudoku.txt" >>= FE.embedFile)

parseData :: String -> [Grid]
parseData =
    (map . map . map) digitToInt . map (drop 1) . partitionByStep 10 10 . lines

findSolution :: Grid -> Maybe Grid
findSolution = find isCompleted . dfs makeTentativeGrids

isCompleted :: Grid -> Bool
isCompleted = (all . all) (/= 0)

dfs :: (Grid -> [Grid]) -> Grid -> [Grid]
dfs f grid = grid : (f grid >>= dfs f)

makeTentativeGrids :: Grid -> [Grid]
makeTentativeGrids g =
    map (replaceGrid g pos) numbers
  where
    (pos, numbers) =
        minimumBy (compare `on` length . snd)
            . map (\x -> (x, candidateNumbers g x))
            $ undeterminedPositions g

candidateNumbers :: Grid -> (Int, Int) -> [Int]
candidateNumbers g (r, c) =
    filter (`notElem` neighbors) [1 .. 9]
  where
    neighbors =
        concatMap (\f -> f g (r, c)) [numbersInRow, numbersInCol, numbersInBox]

numbersInRow :: Grid -> (Int, Int) -> [Int]
numbersInRow g (r, _) = g !! r

numbersInCol :: Grid -> (Int, Int) -> [Int]
numbersInCol g (_, c) = map (!! c) g

numbersInBox :: Grid -> (Int, Int) -> [Int]
numbersInBox g (r, c) =
    (take 3 . drop ((r `div` 3) * 3)) g >>= take 3 . drop ((c `div` 3) * 3)

undeterminedPositions :: Grid -> [(Int, Int)]
undeterminedPositions g = do
    (r, row) <- addIndex g
    (c, v) <- addIndex row
    guard (v == 0)
    pure (r, c)
  where
    addIndex :: [a] -> [(Int, a)]
    addIndex = zip [0 ..]

replaceGrid :: Grid -> (Int, Int) -> Int -> Grid
replaceGrid g (r, c) v =
    insertBetween row1 (replaceLst (headExn row2) c v) (tailExn row2)
  where
    (row1, row2) = splitAt r g

replaceLst :: [a] -> Int -> a -> [a]
replaceLst lst idx v =
    insertBetween l1 v (drop 1 l2)
  where
    (l1, l2) = splitAt idx lst

insertBetween :: [a] -> a -> [a] -> [a]
insertBetween l1 v l2 = l1 ++ (v : l2)

get3digitNumber :: Grid -> Int
get3digitNumber g =
    foldl (\acc n -> acc * 10 + n) 0
        . take 3
        $ headExn g

compute :: String
compute =
    show
        . sum
        . map (get3digitNumber . fromJust . findSolution)
        $ parseData (BS.unpack fileData)

solve :: String
solve = compute
