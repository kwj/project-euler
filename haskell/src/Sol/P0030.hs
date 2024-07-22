module Sol.P0030 (compute, solve) where

import Data.Array.Unboxed (UArray, listArray, (!))
import Data.List (sort)

import Mylib.Combinatorics (combinationsWithRepetition)
import Mylib.Util (digits)

compute :: Int -> String
compute p =
    show . sum $ map aux [2 .. 6]
  where
    powerTbl :: UArray Int Int
    powerTbl = listArray (0, 9) (map (^ p) [0 .. 9])

    aux :: Int -> Int
    aux k =
        sum
            . map fst
            . filter (\(n, lst) -> (sort $ digits n) == lst)
            . map (\lst -> (sum $ map (powerTbl !) lst, lst))
            $ combinationsWithRepetition k [0 .. 9]

solve :: String
solve = compute 5
