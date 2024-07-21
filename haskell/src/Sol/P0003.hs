module Sol.P0003 (compute, solve) where

import Mylib.Factor (primeFactors)
import Mylib.Util (lastExn)

compute :: Int -> String
compute = show . lastExn . primeFactors

solve :: String
solve = compute 600851475143
