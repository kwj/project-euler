module Sol.P0040 (compute, solve) where

import Mylib.Util (digits, headExn)

getBlock :: Int -> (Int, Int)
getBlock nth =
    headExn . snd $ break (\(_, y) -> nth <= y) blocks
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
    show $ product $ map d [1, 10, 100, 1_000, 10_000, 100_000, 1_000_000]

solve :: String
solve = compute
