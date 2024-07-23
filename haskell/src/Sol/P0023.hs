module Sol.P0023 (compute, solve) where

import Control.Monad (when)
import Data.Array.ST (newArray, runSTUArray, writeArray)
import Data.Array.Unboxed (UArray, (!))
import Data.Foldable (for_)

import Mylib.Factor (aliquotSumTbl)

abndntTbl :: Int -> UArray Int Bool
abndntTbl limit = runSTUArray $ do
    tbl <- newArray (0, limit) False
    for_ [0 .. limit] $ \x -> do
        when (x < alqTbl ! x) (writeArray tbl x True)
    pure tbl
  where
    alqTbl = aliquotSumTbl limit

compute :: Int -> String
compute limit =
    show $ aux [1 .. limit] [] 0
  where
    tbl = abndntTbl limit

    aux :: [Int] -> [Int] -> Int -> Int
    aux [] _ acc = acc
    aux (x : xs) ab_lst acc
        | even x && tbl ! ab_cand =
            if any (\a -> tbl ! (x - a)) (ab_cand : ab_lst)
                then aux xs (ab_cand : ab_lst) acc
                else aux xs (ab_cand : ab_lst) (acc + x)
        | otherwise =
            if any (\a -> tbl ! (x - a)) ab_lst
                then aux xs ab_lst acc
                else aux xs ab_lst (acc + x)
      where
        ab_cand = x `div` 2

solve :: String
solve = compute 28_123
