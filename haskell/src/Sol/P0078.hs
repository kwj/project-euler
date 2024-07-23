module Sol.P0078 (compute, solve) where

{-
https://en.wikipedia.org/wiki/Partition_(number_theory)
https://en.wikipedia.org/wiki/Partition_function_(number_theory)
https://en.wikipedia.org/wiki/Pentagonal_number_theorem

  p(n) = Sigma{k ∈ Z/{0}} (-1)^(k+1) * p(n - k(3k-1)/2)
       = p(n-1) + p(n-2) - p(n-5) - p(n-7) + p(n-12) + p(n-15) - p(n-22) - ...

    [p(0) = 1, p(k) = 0 when k < 0]
-}

import Data.Array.IArray (Array, assocs, bounds, elems, listArray, (!))
import Data.Foldable (asum)
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

    p :: Int -> Int
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
        . asum
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
