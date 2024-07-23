module Sol.P0039 (compute, solve) where

{-
Primitive Pythagorean triples (variant type)
  https://en.wikipedia.org/wiki/Pythagorean_triple#A_variant

`m` > `n` > 0.
`m` and `n` are both odd and coprime.
   hypotenuse: (m^2 + n^2) / 2
   catheti: mn, (m^2 - n^2) / 2
   perimeter: mn + (m^2 - n^2) / 2 + (m^2 + n^2) / 2 = m(m + n)
-}

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
