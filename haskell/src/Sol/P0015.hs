module Sol.P0015 (compute, solve) where

import Mylib.Math (binomial)

compute :: Int -> Int -> String
compute x y = show $ binomial (x + y) y

solve :: String
solve = compute 20 20
