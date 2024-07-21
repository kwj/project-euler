module Sol.P0033 (compute, solve) where

import Data.Ratio (denominator, (%))

import Mylib.Combinatorics (combinations)

compute :: String
compute =
    show . denominator $ product fractions
  where
    fractions =
        [ n % d
        | [n, d, x] <- combinations 3 [1 .. 9 :: Int]
        , 9 * n * (x - d) == x * (d - n)
        ]

solve :: String
solve = compute
