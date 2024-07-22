module Sol.P0024 (compute, solve) where

import Text.Printf (printf)

import Mylib.Math (factorial)

compute :: Int -> String
compute nth =
    printf "%010d" $ aux [0 .. 9] (pred nth) 0
  where
    aux :: [Int] -> Int -> Int -> Int
    aux [] _ acc = acc
    aux xs idx acc =
        aux (hd ++ (drop 1 tl)) (idx `mod` blk) (10 * acc + xs !! cnt)
      where
        blk = factorial (pred $ length xs)
        cnt = idx `div` blk
        (hd, tl) = splitAt cnt xs

solve :: String
solve = compute 1_000_000
