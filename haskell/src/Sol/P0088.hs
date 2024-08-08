module Sol.P0088 (compute, solve) where

{-
  N(k) = a1 + a2 + ... + ak = a1 * a2 * ... * ak
    N(k) must be composite numbers.
  min_N(k): minimal product-sum N(k)

  when k = 2
    sum {2,2} = prod {2,2}
  when k > 2 and {a1, a2, a3, ..., ak}
    min sum = sum {1, 1, ..., 1} = k
    --> min_N(k) >= k
  when k > 2 and {a1, a2, a3, ..., ak} = {1, ..., 1, 2, k}
    for all k>2, there exists Ak = {a1, a2, ..., ak} = {1, 1, ..., 1, 2, k}, and
    prod Ak = sum Ak = N(k) = 2k
    --> min_N(k) <= 2k

  2 <= k <= 12000
   --> k <= N(k) <= 24000
-}

import Control.Monad (when)
import Control.Monad.ST (ST)
import Data.Array.ST (STUArray, modifyArray, newArray, runSTUArray)
import Data.Array.Unboxed (UArray, elems)
import Data.Foldable (for_)
import Data.List (nub)

prodSumLst :: Int -> UArray Int Int
prodSumLst limit =
    runSTUArray $ do
        tbl <- newArray (1, limit) upper_value
        aux 1 0 0 2 tbl
        pure tbl
  where
    upper_value = limit * 2

    aux :: forall s. Int -> Int -> Int -> Int -> STUArray s Int Int -> ST s ()
    aux p s len n tbl =
        when (k <= limit) $ do
            modifyArray tbl k (min p)
            for_
                [n .. upper_value `div` p]
                (\x -> aux (p * x) (s + x) (len + 1) x tbl)
      where
        k = p - s + len

compute :: Int -> String
compute limit =
    show
        . sum
        . nub
        . drop 1
        . elems
        $ prodSumLst limit

solve :: String
solve = compute 12_000
