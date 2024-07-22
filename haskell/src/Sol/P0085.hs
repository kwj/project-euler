module Sol.P0085 (compute, solve) where

import Data.Function (on)
import Data.List (minimumBy)
import Data.Maybe (fromJust)

import Mylib.Math (isqrt)
import Mylib.Util (headExn)

getApproximation :: Int -> Int -> Maybe (Int, Int)
getApproximation m target
    | m >= n =
        Nothing
    | otherwise =
        let diff_1 = abs (target - lhs m (n - 1))
            diff_2 = abs (target - lhs m n)
         in if diff_1 < diff_2
                then Just (diff_1, m * (n - 1))
                else Just (diff_2, m * n)
  where
    n = headExn $ dropWhile (\x -> target - lhs m x > 0) [isqrt (target `div` (m * (m + 1))) - 1 ..]

    lhs :: Int -> Int -> Int
    lhs x y = x * (x + 1) * y * (y + 1)

compute :: Int -> String
compute target =
    show
        . snd
        . minimumBy (compare `on` fst)
        . map fromJust
        . takeWhile (/= Nothing)
        $ map (\m -> getApproximation m (target * 4)) [1 ..]

solve :: String
solve = compute 2_000_000
