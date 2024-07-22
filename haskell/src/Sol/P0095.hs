module Sol.P0095 (compute, solve) where

-- [TODO] This implementation is slow and needs to be improved.

import Data.Array.Unboxed ((!))
import Data.Function (on)
import Data.List (maximumBy)
import Data.Maybe (fromJust)

import Mylib.Factor (aliquotSumTbl)

-- Note: This result contains not only amicable chains,
--       but also perfect numbers and amicable pairs.
makeAmicableChains :: Int -> [[Int]]
makeAmicableChains limit =
    aux [2 .. limit] []
  where
    nextPosTbl = aliquotSumTbl limit

    aux :: [Int] -> [Maybe [Int]] -> [[Int]]
    aux [] chains = map fromJust $ filter (/= Nothing) chains
    aux (x : xs) chains =
        aux xs ((go x []) : chains)

    go :: Int -> [Int] -> Maybe [Int]
    go n chain
        | n <= 1 || n > limit =
            Nothing
        | elem n chain == True =
            let (l1, _) = splitLst chain n []
             in Just l1
        | otherwise =
            go (nextPosTbl ! n) (n : chain)

    splitLst :: [Int] -> Int -> [Int] -> ([Int], [Int])
    splitLst [] _ left = (reverse left, [])
    splitLst (x : xs) n left =
        if x == n
            then
                (reverse (x : left), xs)
            else
                splitLst xs n (x : left)

compute :: Int -> String
compute limit =
    show
        . minimum
        . maximumBy (compare `on` length)
        $ makeAmicableChains limit

solve :: String
solve = compute 1_000_000
