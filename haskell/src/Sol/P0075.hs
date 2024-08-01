module Sol.P0075 (compute, solve) where

{-
Pythagorean triple

  a = k * (m^2 - n^2), b = k * 2mn, c = k * (m^2 + n^2)
    where m > n > 0, gcd(m, n) = 1

  perimeter L = k * (2m^2 + 2mn)
              = k * 2m(m + n)

  2m(m + n) = L/k
    -->
  2m^2 < 2m(m + n) = L/k
    <-->
  m^2 < L/2k

  'm' is maximized when k=1
    max(m) < sqrt(L/2)
-}

import Data.Array.Unboxed (UArray, accumArray, elems)

import Mylib.Math (isqrt)

compute :: Int -> String
compute limit =
    show
        . length
        . filter (== 1)
        . elems
        . (accumArray (+) 0 (1, limit) :: [(Int, Int)] -> UArray Int Int)
        . map (, 1)
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
