module Sol.P0050 (compute, solve) where

import Data.List (findIndex)
import Data.Maybe (fromJust)

import Mylib.Prime (isPrime, primeNumbers)

-- 0, 2, 5, 10, 17, 28, 41, 58, 77, 100, 129, ...
cumsumPrimes :: [Int]
cumsumPrimes = scanl (+) 0 primeNumbers

compute :: Int -> String
compute limit =
    show $ go 0 max_window_size
  where
    max_window_size = pred . fromJust $ findIndex (>= limit) cumsumPrimes

    go :: Int -> Int -> Int
    go left k
        | diff >= limit =
            go 0 (pred k)
        | isPrime diff =
            diff
        | otherwise =
            go (succ left) k
      where
        diff = cumsumPrimes !! (left + k) - cumsumPrimes !! left

solve :: String
solve = compute 1_000_000
