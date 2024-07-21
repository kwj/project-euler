module Sol.P0044 (compute, solve) where

import Mylib.Factor (pfactorsToDivisors, primeFactors)
import Mylib.Math (isPentagonal)
import Mylib.Util (headExn)

getDivisors :: Int -> [Int]
getDivisors n =
    filter (\x -> x < n && mod x 3 == mod n 3)
        . pfactorsToDivisors
        $ primeFactors n ++ primeFactors (3 * n - 1)

pent :: Int -> Int
pent n = n * (3 * n - 1) `div` 2

checkConditions :: Int -> Bool
checkConditions d =
    any isSumPentagonal pairs
  where
    lhs = d * (3 * d - 1)
    pairs =
        [ (r1, r2)
        | r1 <- getDivisors d
        , let r2 = lhs `div` r1
        , mod r2 3 == 2
        , mod (r1 + (r2 + 1) `div` 3) 2 == 0
        ]
    isSumPentagonal :: (Int, Int) -> Bool
    isSumPentagonal (a, b) =
        isPentagonal (pent k + pent j)
      where
        k = (a + (b + 1) `div` 3) `div` 2
        j = k - a

compute :: String
compute =
    show . pent . headExn $ filter checkConditions [4 ..]

solve :: String
solve = compute
