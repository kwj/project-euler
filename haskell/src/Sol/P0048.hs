module Sol.P0048 (compute, solve) where

import Text.Printf (printf)

import Mylib.Math (powerModExn)

compute :: Int -> String
compute limit =
    printf "%010d"
        . foldl1 (\acc x -> (acc + x) `mod` modulo)
        $ map (\x -> powerModExn x x modulo) [1 .. limit]
  where
    modulo = 10 ^ (10 :: Int)

solve :: String
solve = compute 1_000
