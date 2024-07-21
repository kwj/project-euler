module Sol.P0052 (compute, solve) where

import Data.List (sort)

import Mylib.Util (digits, headExn)

isPermutedMultiples :: Int -> Bool
isPermutedMultiples n =
    all
        (== (sort $ digits n))
        (map (\x -> sort $ digits (n * x)) [2 .. 6])

compute :: String
compute =
    show
        . headExn
        . filter isPermutedMultiples
        $ concatMap (\x -> [10 ^ (x - 1) .. (10 ^ x `div` 6)]) [6 :: Int ..]

solve :: String
solve = compute
