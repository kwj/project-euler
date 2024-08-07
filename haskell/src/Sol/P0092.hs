module Sol.P0092 (compute, solve) where

import Data.List (group)

import Mylib.Combinatorics (combinationsWithRepetition)
import Mylib.Math (factorial, numOfDigits)
import Mylib.Util (digits)

isGroup89 :: Int -> Bool
isGroup89 n
    | n /= 89 && n > 1 =
        isGroup89 (sum $ (\x -> x * x) <$> digits n)
    | otherwise =
        n == 89

compute :: Int -> String
compute limit =
    show
        . sum
        $ (numerator `div`) <$> denominators
  where
    ndigits = (numOfDigits limit 10) - 1
    squares = [0, 1, 4, 9, 16, 25, 36, 49, 64, 81]
    denominators =
        [ product . map (factorial . length) . group $ pat
        | pat <- combinationsWithRepetition ndigits squares
        , isGroup89 (sum pat)
        ]
    numerator = factorial ndigits

solve :: String
solve = compute 10_000_000 -- it must be a power of 10 in this solver
