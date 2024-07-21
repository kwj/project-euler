module Sol.P0046 (compute, solve) where

import Mylib.Math (isqrt)
import Mylib.Prime (isPrime)

isTwicedSquare :: Int -> Bool
isTwicedSquare n =
    tmp * tmp == n `div` 2
  where
    tmp = isqrt (n `div` 2)

compute :: String
compute =
    show $ aux 9 [7, 5, 3]
  where
    aux :: Int -> [Int] -> Int
    aux x oddPrimes
        | isPrime x == True = aux (x + 2) (x : oddPrimes)
        | otherwise =
            if any (\p -> isTwicedSquare (x - p)) oddPrimes
                then aux (x + 2) oddPrimes
                else x

solve :: String
solve = compute
