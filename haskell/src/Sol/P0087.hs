module Sol.P0087 (compute, solve) where

-- [Note] This implementation is slow.

import qualified Data.IntSet as S (fromList, size)

import Mylib.Prime (primeNumbers)

compute :: Int -> String
compute limit =
    show
        . S.size
        $ S.fromList
            [ pow x 2 + pow y 3 + pow z 4
            | x <- takeWhile (\n -> pow n 2 <= limit) primeNumbers
            , y <- takeWhile (\n -> pow x 2 + pow n 3 <= limit) primeNumbers
            , z <- takeWhile (\n -> pow x 2 + pow y 3 + pow n 4 <= limit) primeNumbers
            ]
  where
    pow :: Int -> Int -> Int
    pow b e = b ^ e

solve :: String
solve = compute 50_000_000
