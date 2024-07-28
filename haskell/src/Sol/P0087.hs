module Sol.P0087 (compute, solve) where

-- [Note] This implementation is slow.

import qualified Data.IntSet as S (fromList, size)

import Mylib.Prime (primeNumbers)

compute :: Int -> String
compute limit =
    show
        . S.size
        $ S.fromList
            [ x + y + z
            | x <- [pow p 2 | p <- takeWhile (\n -> pow n 2 < limit) primeNumbers]
            , y <- [pow p 3 | p <- takeWhile (\n -> x + pow n 3 < limit) primeNumbers]
            , z <- [pow p 4 | p <- takeWhile (\n -> x + y + pow n 4 <= limit) primeNumbers]
            ]
  where
    pow :: Int -> Int -> Int
    pow b e = b ^ e

solve :: String
solve = compute 50_000_000
