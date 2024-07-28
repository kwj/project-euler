module Sol.P0074 (compute, solve) where

import Data.Array.Unboxed (UArray, listArray, (!))
import Data.List (group)

import qualified Data.IntSet as S (IntSet, empty, insert, member, size)

import Mylib.Combinatorics (combinationsWithRepetition)
import Mylib.Math (factorial)
import Mylib.Util (headExn)

factTbl :: UArray Int Int
factTbl = listArray (0, 9) [1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880]

nextNumber :: Int -> Int
nextNumber = aux 0
  where
    aux :: Int -> Int -> Int
    aux acc 0 = acc
    aux acc x = aux (acc + factTbl ! (x `mod` 10)) (x `div` 10)

chainLength :: Int -> Int
chainLength n =
    aux S.empty n
  where
    aux :: S.IntSet -> Int -> Int
    aux set x
        | S.member x set = S.size set
        | otherwise = aux (S.insert x set) (nextNumber x)

countNumbers :: [Int] -> Int
countNumbers [] = error "fatal error (unreachable)"
countNumbers lst
    | headExn lst /= 0 =
        factorial (length lst) `div` denom lst
    | otherwise =
        factorial (length lst) `div` denom lst
            - (factorial $ length (drop 1 lst)) `div` denom (drop 1 lst)
  where
    denom = foldl (\acc x -> acc * factorial (length x)) 1 . group

compute :: Int -> String
compute max_digit =
    show
        . sum
        . map countNumbers
        . filter (\lst -> (chainLength . sum $ (factTbl !) <$> lst) == max_chain - 1)
        $ flip combinationsWithRepetition numbers =<< [1 .. max_digit]
  where
    numbers = [0 :: Int .. 9]
    max_chain = 60

solve :: String
solve = compute 6

{-
-- The following is a simple method, though less efficient.

compute :: String
compute =
    show
        . length
        . filter (== max_chain)
        $ map chainLength [1 .. (1_000_000 - 1)]
  where
    max_chain = 60
-}
