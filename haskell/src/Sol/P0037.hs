module Sol.P0037 (compute, solve) where

import Data.List (unfoldr)

import Mylib.Math (numOfDigits)
import Mylib.Prime (isPrime)

addPrefix :: [Int] -> [Int] -> [Int]
addPrefix prefix candsLst =
    [p * (10 ^ numOfDigits n 10) + n | p <- prefix, n <- candsLst]

pickupPrimes :: [Int] -> [Int]
pickupPrimes lst =
    filter isRightTruncablePrime (addPrefix [2, 3, 5, 7] lst)
  where
    isRightTruncablePrime :: Int -> Bool
    isRightTruncablePrime 0 = False
    isRightTruncablePrime n =
        all isPrime (unfoldr (\x -> if x == 0 then Nothing else Just (x, div x 10)) n)

nextPrimes :: [Int] -> [Int]
nextPrimes lst = filter isPrime (addPrefix [1, 3, 7, 9] lst)

compute :: String
compute =
    show . sum $ aux [] [3, 7]
  where
    aux :: [Int] -> [Int] -> [Int]
    aux result cands
        | length result == 11 = result
        | otherwise = aux (result ++ pickupPrimes cands) (nextPrimes cands)

solve :: String
solve = compute
