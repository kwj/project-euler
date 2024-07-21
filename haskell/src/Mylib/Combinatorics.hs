module Mylib.Combinatorics (
    powerset,
    cartesianProduct,
    permutations,
    permutationsWithRepetition,
    combinations,
    combinationsWithRepetition,
) where

import Control.Monad (filterM)

powerset :: [a] -> [[a]]
powerset = filterM (const [True, False])

cartesianProduct :: (Traversable t, Monad m) => t (m a) -> m (t a)
cartesianProduct = sequence

-- The following methods are naively, though the order of
-- these output is lexicographic.

_choose :: [a] -> [a] -> [(a, [a])]
_choose _ [] = []
_choose left (x : xs) = (x, reverse left ++ xs) : _choose (x : left) xs

permutations :: Int -> [a] -> [[a]]
permutations 0 _ = [[]]
permutations _ [] = []
permutations n lst =
    [x : y | (x, xs) <- _choose [] lst, y <- permutations (n - 1) xs]

permutationsWithRepetition :: Int -> [a] -> [[a]]
permutationsWithRepetition 0 _ = [[]]
permutationsWithRepetition _ [] = []
permutationsWithRepetition n lst =
    [x : y | (x, _) <- _choose [] lst, y <- permutationsWithRepetition (n - 1) lst]

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x : xs) =
    [x : y | y <- combinations (n - 1) xs] ++ combinations n xs

combinationsWithRepetition :: Int -> [a] -> [[a]]
combinationsWithRepetition 0 _ = [[]]
combinationsWithRepetition _ [] = []
combinationsWithRepetition n lst@(x : xs) =
    [x : y | y <- combinationsWithRepetition (n - 1) lst]
        ++ combinationsWithRepetition n xs
