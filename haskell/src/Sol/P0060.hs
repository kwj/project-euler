module Sol.P0060 (compute, solve) where

import Control.Monad (MonadPlus, mplus, mzero)

import qualified Data.IntMap as M (IntMap, empty, insert, (!))
import qualified Data.IntSet as S (IntSet, fromList, member)

import Mylib.Math (numOfDigits)
import Mylib.Prime (isPrime, primeNumbers)

{-
This implementation is a bit slow.
The following is a result on my Raspberry Pi 4.

% cabal v2-run pe-solver -- 60
[Problem 60]
Answer: 26033
Elapsed time: 9.548267 sec.
-}

-- [3, 7, 13, 19, ...]
primes_rem1 :: [Int]
primes_rem1 = 3 : filter (\p -> p `mod` 3 == 1) (drop 3 primeNumbers)

-- [3, 11, 17, 23, ...]
primes_rem2 :: [Int]
primes_rem2 = 3 : filter (\p -> p `mod` 3 == 2) (drop 3 primeNumbers)

pairablePrimes :: Int -> Int -> [Int]
pairablePrimes p limit =
    [x | x <- takeWhile (\x -> x + p < limit && x < p) ascPrimes, isPair x]
  where
    upper_p = 10 ^ (numOfDigits p 10)
    ascPrimes =
        if p `mod` 3 == 1
            then primes_rem1
            else primes_rem2

    isPair :: Int -> Bool
    isPair x =
        isPrime (x * upper_p + p) && isPrime (p * upper_x + x)
      where
        upper_x = 10 ^ (numOfDigits x 10)

findCliques :: [Int] -> Int -> M.IntMap S.IntSet -> [[Int]]
findCliques dscNbrs size tbl =
    aux [([], dscNbrs)]
  where
    aux :: MonadPlus m => [([Int], [Int])] -> m [Int]
    aux [] = mzero
    aux ((clq, rest) : tpls)
        | length clq == size =
            pure clq `mplus` aux tpls
        | otherwise =
            let next_cands = filter (\x -> all (\y -> S.member x (tbl M.! y)) clq) rest
                next_tpls =
                    filter
                        (\tpl -> length (snd tpl) >= size - length clq - 1)
                        (zip ((: clq) <$> next_cands) (flip drop next_cands <$> [1 ..]))
             in aux (next_tpls ++ tpls) -- Depth-first search

compute :: Int -> String
compute groupSize =
    show $ aux maxBound M.empty (drop 3 primeNumbers)
  where
    updateMinSum :: Int -> [[Int]] -> Int -> Int
    updateMinSum p cliques current =
        min current (minimum $ (\clq -> sum $ p : clq) <$> cliques)

    aux :: Int -> M.IntMap S.IntSet -> [Int] -> Int
    aux _ _ [] = error "fatal error (unreachable)"
    aux minSum tbl (p : ps)
        | p >= minSum =
            -- All prime numbers under the `minSum` have been checked,
            -- it is confirmed that the `minSum` is the smallest sum.
            minSum
        | otherwise =
            -- If not, search for prime numbers less than 'p'
            -- that can be connected to 'p'.
            let nbrs = pairablePrimes p minSum
                new_tbl = M.insert p (S.fromList nbrs) tbl
             in if length nbrs < groupSize - 1
                    then
                        aux minSum new_tbl ps
                    else
                        let cliques = findCliques (reverse nbrs) (groupSize - 1) tbl
                         in if null cliques
                                then aux minSum new_tbl ps
                                else aux (updateMinSum p cliques minSum) new_tbl ps

solve :: String
solve = compute 5
