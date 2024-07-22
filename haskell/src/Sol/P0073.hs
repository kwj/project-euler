module Sol.P0073 (compute, solve) where

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
f x = sum $ map (\j -> (j - 1) `div` 2 - j `div` 3) [1 .. x]

g :: Int -> Int
g n = sum $ map (\k -> mbTbl ! k * f (n `div` k)) [1 .. n]
  where
    mbTbl = mobiusTbl n

compute :: Int -> String
compute = show . g

solve :: String
solve = compute 12_000
