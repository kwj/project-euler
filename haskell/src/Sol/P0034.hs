module Sol.P0034 (compute, solve) where

{-
the maximum n-digits number is n * 9!

  10 ** (n-1) <= n * 9!
  -> n - 1 <= log10(n * 9!)
  -> n - 1 <= log10(n) + log10(9!)
  -> n - 1 <= log10(n) + 5.559
  -> n <= log10(n) + 6.559

  >>> math.log10(5)
  0.6989700043360189
  >>> math.log10(6)
  0.7781512503836436
  >>> math.log10(7)
  0.8450980400142568
  >>> math.log10(8)
  0.9030899869919435

so, 'n' is equal to or less than 7.
-}

import Data.Array.Unboxed (UArray, listArray, (!))
import Data.List (sort)

import Mylib.Combinatorics (combinationsWithRepetition)
import Mylib.Math (factorial)
import Mylib.Util (digits)

compute :: String
compute =
    show . sum $ aux <$> [2 .. 7] -- 2-digit .. 7-digit numbers
  where
    factTbl :: UArray Int Int
    factTbl = listArray (0, 9) (factorial <$> [0 .. 9])
    aux k =
        sum
            . map fst
            . filter (\(n, lst) -> (sort $ digits n) == lst)
            . map (\lst -> (sum $ (factTbl !) <$> lst, lst))
            $ combinationsWithRepetition k [0 .. 9]

solve :: String
solve = compute
