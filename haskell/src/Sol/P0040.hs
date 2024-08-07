module Sol.P0040 (compute, solve) where

{-
  0.123456789 | 10111213...979899 | 100101102...997998999  | 100010011002  ...
    ---------   -----------------   ---------------------   -----------------
len: 1 * 9       2 * 90              3 * 900                 4 * 9000      ...
     1 * 9 * 1   2 * 9 * 10          3 * 9 * 100             4 * 9 * 1000  ...
       --> block_num * 9 * base

block #1: 1-digit number
block #2: 2-digits number
block #3: 3-digits number
  ...
block #n: n-digits number
-}

import Mylib.Util (digits, headExn)

getBlock :: Int -> (Int, Int)
getBlock nth =
    headExn . snd $ break ((nth <=) . snd) blocks
  where
    blocks :: [(Int, Int)]
    blocks = iterate (\(x, y) -> (x + 1, y + (x + 1) * 9 * 10 ^ x)) (1, 9)

d :: Int -> Int
d nth =
    digits n !! idx
  where
    (ndigits, maxNth) = getBlock nth
    n = 10 ^ ndigits - 1 - (div (maxNth - nth) ndigits)
    idx = mod (maxNth - nth) ndigits

compute :: String
compute =
    show . product $ d <$> [1, 10, 100, 1_000, 10_000, 100_000, 1_000_000]

solve :: String
solve = compute
