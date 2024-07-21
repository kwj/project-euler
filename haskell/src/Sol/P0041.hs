module Sol.P0041 (compute, solve) where

import Mylib.Combinatorics (permutations)
import Mylib.Prime (isPrime)
import Mylib.Util (headExn, undigits)

compute :: String
compute =
    show
        . headExn
        . filter isPrime
        $ map (undigits . reverse) digitsLst
  where
    digitsLst = permutations 7 [7, 6, 5, 4, 3, 2, 1] ++ permutations 4 [4, 3, 2, 1]

solve :: String
solve = compute
