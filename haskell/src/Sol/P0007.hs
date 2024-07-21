module Sol.P0007 (compute, solve) where

import Mylib.Prime (primeNumbers)

compute :: Int -> String
compute nth
    | nth <= 0 = error "The argument must be positive"
    | otherwise = show $ primeNumbers !! (pred nth)

solve :: String
solve = compute 10_001
