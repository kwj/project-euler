module Sol.P0077 (compute, solve) where

import Control.Arrow ((&&&))
import Data.List (find)
import Data.Maybe (fromJust)

import Mylib.Prime (primeNumbers)

partitionByCoins :: [Int] -> Int -> Int
partitionByCoins [] _ = error "fatal error (unreached)"
partitionByCoins coins@(c : cs) rest
    | rest == 0 =
        1
    | rest < 0 || c > rest =
        0
    | otherwise =
        partitionByCoins coins (rest - c) + partitionByCoins cs rest

compute :: Int -> String
compute thr =
    show
        . fst
        . fromJust
        . find ((> thr) . snd)
        $ (id &&& partitionByCoins primeNumbers) <$> [1 ..]

solve :: String
solve = compute 5_000
