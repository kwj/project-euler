module Sol.P0078 (compute, solve) where

import Control.Monad (msum)
import Data.Array.IArray (Array, assocs, bounds, elems, listArray, (!))
import Data.List (find, unfoldr)
import Data.Maybe (fromJust)

-- The unboxed array (UArray) can't be used to this solution because recursive
-- definition is used. Please see the following link for details.
-- https://wiki.haskell.org/Arrays#Unboxed_arrays

expand_partition_array :: Int -> Array Int Int -> Int -> Array Int Int
expand_partition_array denom old_array new_end =
    new_array
  where
    new_array =
        listArray
            (0, new_end)
            (elems old_array ++ [mod (p x) denom | x <- [old_end + 1 .. new_end]])
    (_, old_end) = bounds old_array
    p n =
        sum $
            zipWith
                (*)
                (cycle [1, 1, -1, -1])
                [new_array ! (n - k) | k <- takeWhile (<= n) gpNumbers]

gpNumbers :: [Int]
gpNumbers =
    unfoldr
        (\(x, y) -> Just (x, (pent y, if y < 0 then succ (negate y) else negate y)))
        (1, -1)
  where
    pent n = n * (3 * n - 1) `div` 2

compute :: Int -> String
compute denom =
    show
        . fst
        . fromJust
        . msum
        . map (find (\tpl -> snd tpl == 0) . assocs)
        $ scanl
            (expand_partition_array denom)
            seed_array
            (iterate (* 2) initial_block_size)
  where
    initial_block_size = 1_000
    seed_array = listArray (0, 0) [1]

solve :: String
solve = compute 1_000_000
