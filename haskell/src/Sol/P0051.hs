module Sol.P0051 (compute, solve) where

import Data.List (elemIndices, sort)

import Mylib.Combinatorics (powerset)
import Mylib.Prime (isPrime, primeNumbers)
import Mylib.Util (digits, headExn, undigits)

-- Here, I used the 'ScopedTypeVariables' extension enabled in GHC2021.
replaceAtIndexes :: forall a. [a] -> [Int] -> a -> [a]
replaceAtIndexes src indexes elm =
    aux (zip [0 ..] src) (filter (>= 0) $ sort indexes) []
  where
    aux :: [(Int, a)] -> [Int] -> [a] -> [a]
    aux [] _ acc = reverse acc
    aux (x : xs) [] acc = aux xs [] ((snd x) : acc)
    aux (x : xs) idx@(i : is) acc
        | fst x == i = aux xs is (elm : acc)
        | otherwise = aux xs idx ((snd x) : acc)

isFamily :: Int -> Int -> Bool
isFamily familySize p =
    any isFamily' [0 .. (10 - familySize)]
  where
    p_digits = digits p

    isFamily' :: Int -> Bool
    isFamily' n =
        any
            ((>= familySize - 1) . length . filter isPrime . cands n)
            (masks n)

    masks :: Int -> [[Int]]
    masks n =
        filter (\mask -> length mask >= 3 && length mask `mod` 3 == 0 && mask !! 0 /= 0)
            . powerset
            $ elemIndices n p_digits

    cands :: Int -> [Int] -> [Int]
    cands n mask =
        undigits . replaceAtIndexes p_digits mask <$> [n + 1 .. 9]

compute :: Int -> String
compute familySize =
    show
        . headExn
        . filter (isFamily familySize)
        $ dropWhile (< 1000) primeNumbers

solve :: String
solve = compute 8
