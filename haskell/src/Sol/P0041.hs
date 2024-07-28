module Sol.P0041 (compute, solve) where

{-
(sum_{1}_{9} n) mod 3 = 45 mod 3 = 0  --> 9-digit pandigital number is a multiple of 3.
(sum_{1}_{8} n) mod 3 = 36 mod 3 = 0  --> 8-digit pandigital number is a multiple of 3.
(sum_{1}_{7} n) mod 3 = 28 mod 3 = 1
(sum_{1}_{6} n) mod 3 = 21 mod 3 = 0  --> 6-digit pandigital number is a multiple of 3.
(sum_{1}_{5} n) mod 3 = 15 mod 3 = 0  --> 5-digit pandigital number is a multiple of 3.
(sum_{1}_{4} n) mod 3 = 10 mod 3 = 1
-}

import Data.List (find)
import Data.Maybe (fromJust)

import Mylib.Combinatorics (permutations)
import Mylib.Prime (isPrime)
import Mylib.Util (undigits)

compute :: String
compute =
    show
        . fromJust -- We already know 2143 is a 4-digit pandigital prime number.
        . find isPrime
        $ (undigits . reverse) <$> digitsLst
  where
    digitsLst = permutations 7 [7, 6, 5, 4, 3, 2, 1] ++ permutations 4 [4, 3, 2, 1]

solve :: String
solve = compute
