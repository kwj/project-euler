module Sol.P0046 (compute, solve) where

import Mylib.Math (isSquare)
import Mylib.Prime (isPrime)

isTwicedSquare :: Int -> Bool
isTwicedSquare n = isSquare (n `div` 2)

compute :: String
compute =
    show $ aux 9 [7, 5, 3]
  where
    aux :: Int -> [Int] -> Int
    aux x oddPrimes
        | isPrime x = aux (x + 2) (x : oddPrimes)
        | otherwise =
            if any (isTwicedSquare . (x -)) oddPrimes
                then aux (x + 2) oddPrimes
                else x

solve :: String
solve = compute
