{-# LANGUAGE TemplateHaskell #-}

-- [TODO] This implementation is slow and needs to be improved.

module Sol.P0096 (compute, solve) where

import Control.Monad (guard)
import Data.Char (digitToInt)
import Data.Function (on)
import Data.List (find, minimumBy)
import Data.Maybe (catMaybes)

import qualified Data.ByteString.Char8 as BS (ByteString, unpack)
import qualified Data.FileEmbed as FE (embedFile, makeRelativeToProject)
import qualified Data.IntSet as S (fromList, notMember)

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
makeTentativeGrids grid =
    replaceGrid grid pos <$> numbers
  where
    (pos, numbers) =
        minimumBy (compare `on` length . snd)
            $ (\x -> (x, candidateNumbers grid x)) <$> undeterminedPositions grid

candidateNumbers :: Grid -> (Int, Int) -> [Int]
candidateNumbers grid (r, c) =
    filter (`S.notMember` neighbors) [1 .. 9]
  where
    neighbors =
        S.fromList $ (\f -> f grid (r, c)) =<< [numbersInRow, numbersInCol, numbersInBox]

numbersInRow :: Grid -> (Int, Int) -> [Int]
numbersInRow grid (r, _) = grid !! r

numbersInCol :: Grid -> (Int, Int) -> [Int]
numbersInCol grid (_, c) = (!! c) <$> grid

numbersInBox :: Grid -> (Int, Int) -> [Int]
numbersInBox grid (r, c) =
    (take 3 . drop ((r `div` 3) * 3)) grid >>= take 3 . drop ((c `div` 3) * 3)

undeterminedPositions :: Grid -> [(Int, Int)]
undeterminedPositions grid = do
    (r, row) <- addIndex grid
    (c, v) <- addIndex row
    guard (v == 0)
    pure (r, c)
  where
    addIndex :: [a] -> [(Int, a)]
    addIndex = zip [0 ..]

replaceGrid :: Grid -> (Int, Int) -> Int -> Grid
replaceGrid grid (r, c) v =
    insertBetween row1 (replaceLst (headExn row2) c v) (tailExn row2)
  where
    (row1, row2) = splitAt r grid

replaceLst :: [a] -> Int -> a -> [a]
replaceLst lst idx v =
    insertBetween l1 v (drop 1 l2)
  where
    (l1, l2) = splitAt idx lst

insertBetween :: [a] -> a -> [a] -> [a]
insertBetween l1 v l2 = l1 ++ (v : l2)

get3digitNumber :: Grid -> Int
get3digitNumber grid =
    foldl (\acc n -> acc * 10 + n) 0
        . take 3
        $ headExn grid

compute :: String
compute =
    show
        . sum
        . map get3digitNumber
        . catMaybes
        $ findSolution <$> parseData (BS.unpack fileData)

solve :: String
solve = compute
