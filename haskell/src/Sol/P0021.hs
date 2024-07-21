module Sol.P0021 (compute, solve) where

import Data.Array.Unboxed ((!))

import Mylib.Factor (aliquotSumTbl)

compute :: Int -> String
compute n =
    show
        . sum
        . map (\x -> x + tbl ! x)
        $ filter (\x -> x > tbl ! x && tbl ! (tbl ! x) == x) [2 .. n]
  where
    tbl = aliquotSumTbl n

solve :: String
solve = compute 10_000
