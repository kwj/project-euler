module Sol.P0033 (compute, solve) where

{-
case #1: (10a + c) / (10c + b) = a / b
case #2: (10a + c) / (10b + c) = a / b
case #3: (10c + a) / (10c + b) = a / b
case #4: (10c + a) / (10b + c) = a / b
// prerequisite: a < b

case #1:
  10ab + bc = 10ac + ab
  -> 9ab = 10ac - bc
  -> 9ab - 9ac = ac - bc
  -> 9a(b - c) = c(a - b)
  -> 9a(c - b) = c(b - a)  [a < b => a < b < c]

case #2:
  10ab + bc = 10ab + ac
  -> bc = ac
  -> b = a   ==> NG (contradiction)

case #3:
  10bc + ab = 10ac + ab
  -> 10bc = 10ac
  -> b = a   ==> NG (contradiction)

case #4:
  10bc + ab = 10ab + ac
  -> 10bc - ac = 9ab
  -> bc - ac = 9ab - 9bc
  -> c(b - a) = 9b(a - c)  [a < b => c < a < b]
  -> a - c = c/9 - ac/9b => 1   ==> NG (bacause c/9 < 1)

We only need to search for the case #1.
-}

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
