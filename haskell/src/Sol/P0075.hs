module Sol.P0075 (compute, solve) where

import Data.Array.Unboxed (UArray, accumArray, elems)

import Mylib.Math (isqrt)

compute :: Int -> String
compute limit =
    show
        . length
        . filter (== 1)
        . elems
        . (accumArray (+) 0 (1, limit) :: [(Int, Int)] -> UArray Int Int)
        . map (\p -> (p, 1))
        $ concat
            [ [perimeter, perimeter * 2 .. limit]
            | m <- [2 .. upper_m]
            , n <- [1 + (m `mod` 2), 1 + (m `mod` 2) + 2 .. (m - 1)]
            , gcd m n == 1
            , let perimeter = 2 * m * (m + n)
            ]
  where
    upper_m = isqrt (limit `div` 2)

solve :: String
solve = compute 1_500_000

{-
-- The following is a naive method and is quite slow.

compute :: Int -> String
compute wireLen =
    show
        . length
        . filter (\lst -> length lst == 1)
        . group
        . sort
        $ concat
            [ [perimeter, perimeter + perimeter .. wireLen]
            | m <- [2 .. limit]
            , n <- [1 + (m `mod` 2), 1 + (m `mod` 2) + 2 .. (m - 1)]
            , gcd m n == 1
            , let perimeter = 2 * m * (m + n)
            ]
  where
    limit = isqrt (wireLen `div` 2)
-}
