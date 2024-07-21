module Sol.P0047 (compute, solve) where

import Data.List (group)

import Mylib.Factor (primeFactors)

compute :: Int -> String
compute len =
    show $ aux 1 0
  where
    aux :: Int -> Int -> Int
    aux x cnt
        | (length . group $ primeFactors x) /= len = aux (succ x) 0
        | cnt == len - 1 = x - len + 1
        | otherwise = aux (succ x) (cnt + 1)

solve :: String
solve = compute 4
