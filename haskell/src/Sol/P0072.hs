module Sol.P0072 (compute, solve) where

import Data.Function (fix)

import Mylib.Math (isqrt)

memoize :: (Int -> a) -> (Int -> a)
memoize f = (map f [0 ..] !!)

sumPhiMemo :: Int -> Int
sumPhiMemo = fix (memoize . sumPhi)

sumPhi :: (Int -> Int) -> Int -> Int
sumPhi f n =
    v - v1 - v2
  where
    v = n * (n + 1) `div` 2
    v1 = sum $ map (\m -> f (n `div` m)) [2 .. (isqrt n)]
    v2 =
        sum $
            map
                (\d -> ((n `div` d) - (n `div` (d + 1))) * f d)
                [1 .. n `div` ((isqrt n) + 1)]

compute :: Int -> String
compute limit =
    show (sumPhiMemo limit - sumPhiMemo 1)

solve :: String
solve = compute 1_000_000
