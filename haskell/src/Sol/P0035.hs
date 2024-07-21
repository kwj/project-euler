module Sol.P0035 (compute, solve) where

import Mylib.Math (numOfDigits)
import Mylib.Prime (isPrime, primes)

compute :: Int -> String
compute limit =
    show $ length $ filter isCircularPrime $ primes 1 limit
  where
    isCircularPrime :: Int -> Bool
    isCircularPrime n =
        all
            isPrime
            (drop 1 . take (k + 1) $ iterate (\x -> (mod x 10) * d + (div x 10)) n)
      where
        k = (numOfDigits n 10) - 1
        d = 10 ^ k

solve :: String
solve = compute 1_000_000
