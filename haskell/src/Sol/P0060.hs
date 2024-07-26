module Sol.P0060 (compute, solve) where

import qualified Data.IntMap as M (IntMap, empty, insert, (!))
import qualified Data.IntSet as S (IntSet, fromList, member)

import Mylib.Combinatorics (combinations)
import Mylib.Math (numOfDigits)
import Mylib.Prime (isPrime, primeNumbers)

{-
This implementation is quite slow. The following is a result on my Raspberry Pi 4.

% cabal v2-run pe-solver -- 60
[Problem 60]
Answer: 26033
Elapsed time: 28.931796 sec.
-}

-- [3, 7, 13, 19, ...]
primes_mod1 :: [Int]
primes_mod1 = 3 : filter (\p -> p `mod` 3 == 1) (drop 3 primeNumbers)

-- [3, 11, 17, 23, ...]
primes_mod2 :: [Int]
primes_mod2 = 3 : filter (\p -> p `mod` 3 == 2) (drop 3 primeNumbers)

pairablePrimes :: Int -> Int -> [Int]
pairablePrimes p limit =
    [x | x <- takeWhile (\x -> x + p < limit && x < p) ascPrimes, isPair x]
  where
    upper_p = 10 ^ (numOfDigits p 10)
    ascPrimes =
        if p `mod` 3 == 1
            then primes_mod1
            else primes_mod2

    isPair :: Int -> Bool
    isPair x =
        isPrime (x * upper_p + p) && isPrime (p * upper_x + x)
      where
        upper_x = 10 ^ (numOfDigits x 10)

-- [TODO] There is room for improvement in this method.
findCliques :: [Int] -> Int -> M.IntMap S.IntSet -> [[Int]]
findCliques dscNbrs size tbl =
    filter isClique cands
  where
    cands = combinations size dscNbrs

    isClique :: [Int] -> Bool
    isClique [] = error "unreachable"
    isClique (x : xs)
        | null xs = True
        | all (`S.member` (tbl M.! x)) xs = isClique xs
        | otherwise = False

compute :: Int -> String
compute groupSize =
    show $ aux maxBound M.empty (drop 3 primeNumbers)
  where
    updateMinSum :: Int -> [[Int]] -> Int -> Int
    updateMinSum p cliques current =
        min current (minimum $ map (\clq -> sum $ p : clq) cliques)

    aux :: Int -> M.IntMap S.IntSet -> [Int] -> Int
    aux _ _ [] = error "fatal error (unreachable)"
    aux minSum tbl (p : ps)
        | p >= minSum =
            -- All prime numbers under the *minSum* have been checked,
            -- this value is confirmed that it is the smallest sum.
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
