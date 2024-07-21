module Sol.P0039 (compute, solve) where

import Data.Function (on)
import Data.List (group, maximumBy, sort)

import Mylib.Math (isqrt)
import Mylib.Util (headExn)

compute :: Int -> String
compute limit =
    show
        . headExn
        . maximumBy (compare `on` length)
        . group
        $ sort perimeters
  where
    perimeters =
        [ p
        | m <- [3, 5 .. isqrt limit]
        , n <- [1, 3 .. m - 1]
        , gcd m n == 1
        , p <- [m * (m + n), m * (m + n) * 2 .. limit]
        ]

solve :: String
solve = compute 1_000
