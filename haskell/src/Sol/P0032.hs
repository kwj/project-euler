module Sol.P0032 (compute, solve) where

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
        | m1 <- [1_000 .. 10_000]
        , m2 <- [2 .. 10]
        , m1 * m2 < 10_000
        ]
    p2 =
        [ (m1 * 10 ^ (6 :: Int) + m2 * 10 ^ (4 :: Int) + m1 * m2, m1 * m2)
        | m1 <- [100 .. 1_000]
        , m2 <- [10 .. 100]
        , m1 * m2 < 10_000
        ]

solve :: String
solve = compute
