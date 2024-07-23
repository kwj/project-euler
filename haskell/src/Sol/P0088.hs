module Sol.P0088 (compute, solve) where

import Control.Monad (when)
import Data.Array.ST (newArray, readArray, runSTUArray, writeArray)
import Data.Array.Unboxed (UArray, elems)
import Data.Foldable (for_)
import Data.List (nub, sort)

prodSumLst :: Int -> UArray Int Int
prodSumLst limit =
    runSTUArray $ do
        tbl <- newArray (1, limit) upper_value
        aux 1 0 0 2 tbl
        pure tbl
  where
    upper_value = limit * 2

    aux p s len n tbl = do
        when (k <= limit) $ do
            v <- readArray tbl k
            writeArray tbl k (min v p)
            for_ [n .. upper_value `div` p]
                (\x -> aux (p * x) (s + x) (len + 1) x tbl)
      where
        k = p - s + len

compute :: Int -> String
compute limit =
    show
        . sum
        . nub
        . sort
        . drop 1
        . elems
        $ prodSumLst limit

solve :: String
solve = compute 12_000
