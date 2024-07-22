module Sol.P0016 (compute, solve) where

import Mylib.Util (digits)

compute :: Int -> String
compute e =
    show . sum $ digits (2 ^ e :: Integer)

solve :: String
solve = compute 1_000
