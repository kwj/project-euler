module Sol.P0023 (compute, solve) where

import Data.Array.ST (newArray, runSTUArray, writeArray)
import Data.Array.Unboxed (UArray, (!))
import Data.Foldable (for_)

import Mylib.Factor (aliquotSumTbl)

abndntTbl :: Int -> UArray Int Bool
abndntTbl limit = runSTUArray $ do
    tbl <- newArray (0, limit) False
    for_ [0 .. limit] $ \x -> do
        if x < alqTbl ! x
            then writeArray tbl x True
            else pure ()
    return tbl
  where
    alqTbl = aliquotSumTbl limit

compute :: Int -> String
compute limit =
    show $ aux [1 .. limit] [] 0
  where
    tbl = abndntTbl limit
    aux :: [Int] -> [Int] -> Int -> Int
    aux [] _ acc = acc
    aux (x : xs) ab_lst acc =
        if even x && tbl ! ab_cand == True
            then
                if any (\a -> tbl ! (x - a)) (ab_cand : ab_lst)
                    then aux xs (ab_cand : ab_lst) acc
                    else aux xs (ab_cand : ab_lst) (acc + x)
            else
                if any (\a -> tbl ! (x - a)) ab_lst
                    then aux xs ab_lst acc
                    else aux xs ab_lst (acc + x)
      where
        ab_cand = x `div` 2

solve :: String
solve = compute 28_123
