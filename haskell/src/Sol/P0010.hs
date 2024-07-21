module Sol.P0010 (compute, solve) where

import Mylib.Prime (primeNumbers)

compute :: Int -> String
compute limit = show . sum $ takeWhile (< limit) primeNumbers

solve :: String
solve = compute 2_000_000
