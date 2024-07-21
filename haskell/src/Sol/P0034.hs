module Sol.P0034 (compute, solve) where

import Data.Array.Unboxed (UArray, listArray, (!))
import Data.List (sort)

import Mylib.Combinatorics (combinationsWithRepetition)
import Mylib.Math (factorial)
import Mylib.Util (digits)

compute :: String
compute =
    show $ sum $ map aux [2 .. 7]
  where
    factTbl :: UArray Int Int
    factTbl = listArray (0, 9) (map factorial [0 .. 9])
    aux k =
        sum
            . map fst
            . filter (\(n, lst) -> (sort $ digits n) == lst)
            . map (\lst -> (sum $ map (factTbl !) lst, lst))
            $ combinationsWithRepetition k [0 .. 9]

solve :: String
solve = compute
