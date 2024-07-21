module Sol.P0097 (compute, solve) where

import Text.Printf (printf)

import Mylib.Math (powerModExn)

compute :: String
compute =
    printf "%010d" $ (28433 * (powerModExn 2 7830457 m) + 1) `mod` m
  where
    m = 10_000_000_000

solve :: String
solve = compute
