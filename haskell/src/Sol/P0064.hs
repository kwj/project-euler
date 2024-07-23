module Sol.P0064 (compute, solve) where

import Mylib.Math (isqrt)

continuedFraction :: Int -> (Int, [Int])
continuedFraction n =
    aux 0 1 isqrt_n []
  where
    isqrt_n = isqrt n
    stop = 2 * isqrt_n

    aux :: Int -> Int -> Int -> [Int] -> (Int, [Int])
    aux b c a lst
        | isqrt_n * isqrt_n == n =
            (isqrt_n, [])
        | a == stop =
            (isqrt_n, reverse lst)
        | otherwise =
            let b' = a * c - b
                c' = (n - b' * b') `div` c
                a' = (isqrt_n + b') `div` c'
             in aux b' c' a' (a' : lst)

compute :: Int -> String
compute limit =
    show
        . length
        . filter (odd . length . snd)
        $ map continuedFraction [1 .. limit]

solve :: String
solve = compute 10_000
