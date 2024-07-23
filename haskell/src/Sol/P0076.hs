module Sol.P0076 (compute, solve) where

{-
another version of problem 31
  coins: 1, 2, 3, ..., 99
  total: 100
-}

import Data.Array.ST (newArray, readArray, runSTUArray, writeArray)
import Data.Array.Unboxed (UArray, (!))
import Data.Foldable (for_)

dpSolve :: Int -> [Int] -> UArray Int Int
dpSolve target coins =
    runSTUArray $ do
        memo <- newArray (0, target) 0
        writeArray memo 0 1
        for_ coins $ \c -> do
            for_ [c .. target] $ \i -> do
                crnt <- readArray memo i
                tmp <- readArray memo (i - c)
                writeArray memo i (crnt + tmp)
        pure memo

compute :: Int -> [Int] -> String
compute target coins =
    show $ (dpSolve target coins) ! target

solve :: String
solve = compute 100 [1 .. 99]
