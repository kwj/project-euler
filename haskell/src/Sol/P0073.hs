module Sol.P0073 (compute, solve) where

{-
f(n): number of fractions a/b, where a < b, b <= n, 1/3 < a/b < 1/2
      --> sigma{i=1, ...,n}((i-1)//2 - i//3)
g(n): number of irreducible fractions a/b, where a < b, b <= n, 1/3 < a/b < 1/2, gcd(a,b)=1

  The answer we should seek is g(12000).

f(n) = sigma{k=1, ..., n}(g(n//k))
 -->
  g(n) = sigma{k=1, ..., n}μ(k)f(n//k)      [möbius inversion formula, μ(): möbius function]
       = sigma{k=1, ..., n}μ(k)sigma{j=1, ..., n//k}((j-1)//2 - j//3)
-}

import Control.Monad (when)
import Control.Monad.ST (ST)
import Data.Array.ST (
    STUArray,
    newArray,
    newGenArray,
    readArray,
    runSTUArray,
    writeArray,
 )
import Data.Array.Unboxed (UArray, (!))
import Data.Foldable (for_)

import Mylib.Math (isqrt)

mobiusTbl :: Int -> UArray Int Int
mobiusTbl limit =
    runSTUArray $ do
        p_tbl <- newGenArray (2, limit) pure :: ST s (STUArray s Int Int)
        for_ [2 .. (isqrt limit)] $ \i -> do
            tmp <- readArray p_tbl i
            when (tmp == i) $ do
                let k = i * i
                for_ [k, k + i .. limit] (\j -> writeArray p_tbl j i)
                for_ [k, k + k .. limit] (\j -> writeArray p_tbl j 0)
        tbl <- newArray (1, limit) 0
        writeArray tbl 1 1
        for_ [2 .. limit] $ \i -> do
            tmp <- readArray p_tbl i
            when (tmp /= 0) $ do
                v <- readArray tbl (i `div` tmp)
                writeArray tbl i (negate v)
        pure tbl

f :: Int -> Int
f x = sum $ (\j -> (j - 1) `div` 2 - j `div` 3) <$> [1 .. x]

g :: Int -> Int
g n = sum $ (\k -> mbTbl ! k * f (n `div` k)) <$> [1 .. n]
  where
    mbTbl = mobiusTbl n

compute :: Int -> String
compute = show . g

solve :: String
solve = compute 12_000
