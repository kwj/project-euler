module Sol.P0032 (compute, solve) where

{-
m * n = mn (multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital)

- numbers of digits of multiplicand/multiplier must be 4 or less.
- if number of digits of multiplicand is 4, number of digits of multiplier is 1.
- if number of digits of multiplicand is 3, number of digits of multiplier is 2.

multiplicand/multiplier/product : 4-digits/1-digit/4-digits or 3-digits/2-digits/4-digits
-}

import Data.List (nub, sort)

import Mylib.Math (isPandigitalNZ)

compute :: String
compute =
    show
        . sum
        . nub
        . sort
        . map snd
        $ filter (\(x, _) -> isPandigitalNZ x) (p1 ++ p2)
  where
    p1 =
        [ (m1 * 10 ^ (5 :: Int) + m2 * 10 ^ (4 :: Int) + m1 * m2, m1 * m2)
        | m1 <- [1_000 .. 9_999]
        , m2 <- [2 .. 9]
        , m1 * m2 < 10_000
        ]
    p2 =
        [ (m1 * 10 ^ (6 :: Int) + m2 * 10 ^ (4 :: Int) + m1 * m2, m1 * m2)
        | m1 <- [100 .. 999]
        , m2 <- [10 .. 99]
        , m1 * m2 < 10_000
        ]

solve :: String
solve = compute
