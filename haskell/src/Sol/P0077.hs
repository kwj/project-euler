module Sol.P0077 (compute, solve) where

import Control.Arrow ((&&&))

import Mylib.Prime (primeNumbers)
import Mylib.Util (headExn)

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
        . headExn
        . dropWhile (\tpl -> snd tpl <= thr)
        $ (id &&& partitionByCoins primeNumbers) <$> [1 ..]

solve :: String
solve = compute 5_000
