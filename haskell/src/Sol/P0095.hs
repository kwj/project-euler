module Sol.P0095 (compute, solve) where

-- [TODO] This implementation is slow and needs to be improved.

import Data.Array.Unboxed ((!))
import Data.Function (on)
import Data.List (elemIndex, maximumBy)
import Data.Maybe (fromJust)

import Mylib.Factor (aliquotSumTbl)

-- Note: This result contains not only amicable chains,
--       but also perfect numbers and amicable pairs.
makeAmicableChains :: Int -> [[Int]]
makeAmicableChains limit =
    map fromJust . filter (/= Nothing) $ aux [] <$> [2 .. limit]
  where
    nextPosTbl = aliquotSumTbl limit

    aux :: [Int] -> Int -> Maybe [Int]
    aux chain n
        | n <= 1 || n > limit =
            Nothing
        | Just idx <- elemIndex n chain =
            Just $ take (idx + 1) chain
        | otherwise =
            aux (n : chain) (nextPosTbl ! n)

compute :: Int -> String
compute limit =
    show
        . minimum
        . maximumBy (compare `on` length)
        $ makeAmicableChains limit

solve :: String
solve = compute 1_000_000
