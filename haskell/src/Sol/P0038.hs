module Sol.P0038 (compute, solve) where

import Mylib.Math (isPandigitalNZ)
import Mylib.Util (headExn)

f :: Int -> Int
f n = n * 100_000 + n * 2

isPandigitalMultiples :: Int -> Bool
isPandigitalMultiples n =
    case mod n 10 of
        0 -> False
        1 -> False
        4 -> False
        5 -> False
        8 -> False
        9 -> False
        _ -> isPandigitalNZ (f n)

compute :: String
compute =
    show . f . headExn $ filter isPandigitalMultiples [9497, 9496 .. 9182]

solve :: String
solve = compute
