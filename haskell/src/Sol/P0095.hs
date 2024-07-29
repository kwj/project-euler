module Sol.P0095 (compute, solve) where

{-
This implementation is slow. The following is a result on my Raspberry Pi 4.

% cabal v2-run pe-solver -- 95
[Problem 95]
Answer: 14316
Elapsed time: 2.747619 sec.
-}

import Control.Arrow ((&&&))
import Data.Array.Unboxed ((!))
import Data.Function (on)
import Data.List (elemIndex, mapAccumL, maximumBy)
import Data.Maybe (catMaybes)

import qualified Data.IntSet as S (IntSet, empty, fromList, member, union)

import Mylib.Factor (aliquotSumTbl)

amicableChains :: Int -> [[Int]]
amicableChains limit =
    catMaybes . snd $ mapAccumL findAmicableChain S.empty [2 .. limit]
  where
    nextPosTbl = aliquotSumTbl limit

    findAmicableChain :: S.IntSet -> Int -> (S.IntSet, Maybe [Int])
    findAmicableChain checked =
        aux checked . checkChain checked

    checkChain :: S.IntSet -> Int -> Either [Int] ([Int], [Int])
    checkChain checked =
        go []
      where
        go :: [Int] -> Int -> Either [Int] ([Int], [Int])
        go chain n
            | n > limit || S.member n checked =
                Left chain
            | Just idx <- elemIndex n chain =
                Right $ (id &&& take (idx + 1)) chain
            | otherwise =
                go (n : chain) (nextPosTbl ! n)

    aux :: S.IntSet -> Either [Int] ([Int], [Int]) -> (S.IntSet, Maybe [Int])
    aux checked (Left x) =
        (S.union checked (S.fromList x), Nothing)
    aux checked (Right (x, loop)) =
        (S.union checked (S.fromList x), Just loop)

compute :: Int -> String
compute limit =
    show
        . minimum
        . maximumBy (compare `on` length)
        $ amicableChains limit

solve :: String
solve = compute 1_000_000
