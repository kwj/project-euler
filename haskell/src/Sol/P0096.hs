{-# LANGUAGE TemplateHaskell #-}

{-
This implementation is slow. The following is a result on my Raspberry Pi 4.

% cabal v2-run pe-solver -- 96
[Problem 96]
Answer: 24702
Elapsed time: 1.963376 sec.
-}

module Sol.P0096 (compute, solve) where

import Control.Applicative (Alternative, empty, (<|>))
import Control.Arrow ((&&&))
import Control.Monad (guard)
import Data.Char (digitToInt)
import Data.Function (on)
import Data.List (minimumBy)
import Data.Maybe (catMaybes)

import qualified Data.ByteString.Char8 as BS (ByteString, unpack)
import qualified Data.FileEmbed as FE (embedFile, makeRelativeToProject)
import qualified Data.IntSet as S (IntSet, fromList, size, toList, (\\))

import Mylib.Util (headExn, partitionByStep, tailExn)

type Grid = [[Int]]
type Pos = (Int, Int)

fileData :: BS.ByteString
fileData = $(FE.makeRelativeToProject "resources/0096_sudoku.txt" >>= FE.embedFile)

parseData :: String -> [Grid]
parseData =
    (map . map . map) digitToInt . map (drop 1) . partitionByStep 10 10 . lines

findSolution :: Grid -> Maybe Grid
findSolution grid =
    dfs makeTentativeGrids [grid]

dfs :: Alternative m => (Grid -> [Grid]) -> [Grid] -> m Grid
dfs _ [] = empty
dfs f (x : xs)
    | isCompleted x =
        pure x <|> dfs f xs
    | otherwise =
        dfs f (f x ++ xs)

isCompleted :: Grid -> Bool
isCompleted = (all . all) (/= 0)

makeTentativeGrids :: Grid -> [Grid]
makeTentativeGrids grid =
    setCandidateNumber grid pos <$> S.toList numbers
  where
    (pos, numbers) =
        minimumBy (compare `on` S.size . snd) $
            (id &&& candidateNumbers grid) <$> undeterminedPositions grid

candidateNumbers :: Grid -> Pos -> S.IntSet
candidateNumbers grid (r, c) =
    allNumbers S.\\ numbersInRow S.\\ numbersInCol S.\\ numbersInBox
  where
    allNumbers = S.fromList [1 .. 9]
    numbersInRow = S.fromList $ grid !! r
    numbersInCol = S.fromList $ (!! c) <$> grid
    numbersInBox =
        S.fromList $
            take 3 . drop ((c `div` 3) * 3) =<< (take 3 . drop ((r `div` 3) * 3)) grid

undeterminedPositions :: Grid -> [Pos]
undeterminedPositions grid = do
    (r, row) <- addIndex grid
    (c, v) <- addIndex row
    guard (v == 0)
    pure (r, c)
  where
    addIndex :: [a] -> [(Int, a)]
    addIndex = zip [0 ..]

setCandidateNumber :: Grid -> Pos -> Int -> Grid
setCandidateNumber grid (r, c) v =
    insertBetween row1 (modifyLst (headExn row2) c v) (tailExn row2)
  where
    (row1, row2) = splitAt r grid

modifyLst :: [a] -> Int -> a -> [a]
modifyLst lst idx v =
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

{-
-- Initial version (for comparison)
-- It's a little slower than the above on my machine.

findSolution :: Grid -> Maybe Grid
findSolution = find isCompleted . dfs makeTentativeGrids

dfs :: (Grid -> [Grid]) -> Grid -> [Grid]
dfs f grid = grid : (f grid >>= dfs f)
-}
