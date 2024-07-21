module Sol.P0073 (compute, solve) where

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
        p_tbl <- newGenArray (2, limit) return :: ST s (STUArray s Int Int)
        for_ [2 .. (isqrt limit)] $ \i -> do
            tmp <- readArray p_tbl i
            if tmp == i
                then
                    let k = i * i
                     in do
                            for_ [k, k + i .. limit] $ (\j -> writeArray p_tbl j i)
                            for_ [k, k + k .. limit] $ (\j -> writeArray p_tbl j 0)
                else pure ()
        tbl <- newArray (1, limit) 0
        writeArray tbl 1 1
        for_ [2 .. limit] $ \i -> do
            tmp <- readArray p_tbl i
            if tmp /= 0
                then do
                    v <- readArray tbl (i `div` tmp)
                    writeArray tbl i (negate v)
                else pure ()
        return tbl

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
