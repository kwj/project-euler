module Sol.P0031 (compute, solve) where

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
        return memo

compute :: Int -> [Int] -> String
compute target coins =
    show $ (dpSolve target coins) ! target

solve :: String
solve = compute 200 [1, 2, 5, 10, 20, 50, 100, 200]
