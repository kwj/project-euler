module Sol.P0078 (compute, solve) where

{-
This implementation is quite slow. The following is a result on my Raspberry Pi 4.
I have no ideas to improve performance so far.

% cabal v2-run pe-solver -- 78
[Problem 78]
Answer: 55374
Elapsed time: 4.809514 sec.
-}

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

-- The unboxed array (UArray) can't be used to this implementation because recursive
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

{-
-- I don't know why the following implementation is slower than the above.

expand_partition_array :: Int -> Array Int Int -> Int -> Array Int Int
expand_partition_array denom old_array new_end =
    runSTArray $ do
        new_array <- newArray (0, new_end) 0
        for_ [0 .. old_end] (\idx -> writeArray new_array idx (old_array ! idx))
        for_ [old_end + 1 .. new_end] $ \idx -> do
            for_ (zip (cycle [1, 1, -1, -1]) [idx - k | k <- takeWhile (<= idx) gpNumbers]) $ \(sign, x) -> do
                crnt <- readArray new_array idx
                tmp <- readArray new_array x
                writeArray new_array idx (mod (crnt + sign * tmp) denom)
        pure new_array
  where
    (_, old_end) = bounds old_array
-}
