module Sol.P0060 (compute, solve) where

import Control.Applicative (Alternative, empty, (<|>))
import Data.List (tails, uncons)
import Data.Maybe (mapMaybe)

import qualified Data.IntMap as M (IntMap, empty, insert, (!))
import qualified Data.IntSet as S (IntSet, fromList, member)

import Mylib.Math (numOfDigits)
import Mylib.Prime (isPrime, primeNumbers)

-- [3, 7, 13, 19, ...]
primes_rem1 :: [Int]
primes_rem1 = 3 : filter ((== 1) . (`mod` 3)) (drop 3 primeNumbers)

-- [3, 11, 17, 23, ...]
primes_rem2 :: [Int]
primes_rem2 = 3 : filter ((== 2) . (`mod` 3)) (drop 3 primeNumbers)

pairablePrimes :: Int -> Int -> [Int]
pairablePrimes p limit =
    [x | x <- takeWhile (\n -> n + p < limit && n < p) ascPrimes, isPair x]
  where
    upper_p = 10 ^ numOfDigits p 10
    ascPrimes =
        if p `mod` 3 == 1
            then primes_rem1
            else primes_rem2

    isPair :: Int -> Bool
    isPair x =
        isPrime (x * upper_p + p) && isPrime (p * upper_x + x)
      where
        upper_x = 10 ^ numOfDigits x 10

findCliques :: [Int] -> Int -> M.IntMap S.IntSet -> [[Int]]
findCliques dscNbrs targetSize tbl =
    dfs findClqs [([], dscNbrs)]
  where
    dfs ::
        Alternative m =>
        (([Int], [Int]) -> [([Int], [Int])]) -> [([Int], [Int])] -> m [Int]
    dfs _ [] = empty
    dfs f (x@(clq, _) : xs)
        | length clq == targetSize =
            pure clq <|> dfs f xs
        | otherwise =
            dfs f (f x ++ xs)

    findClqs :: ([Int], [Int]) -> [([Int], [Int])]
    findClqs (clq, nbrs) =
        map (\(n, lst) -> (n : clq, lst))
            . filter (\(n, _) -> all (S.member n . (tbl M.!)) clq)
            $ cands
      where
        cands =
            mapMaybe
                (\lst -> if length lst >= targetSize - length clq then uncons lst else Nothing)
                $ tails nbrs

compute :: Int -> String
compute groupSize =
    show $ aux maxBound M.empty (drop 3 primeNumbers)
  where
    aux :: Int -> M.IntMap S.IntSet -> [Int] -> Int
    aux _ _ [] = error "fatal error (unreachable)"
    aux minSum tbl (p : ps)
        | p >= minSum =
            -- All prime numbers under the `minSum` have been checked,
            -- and we've confirmed that it is the smallest sum.
            minSum
        | otherwise =
            -- Otherwise, the `minSum` is not confirmed that it's the
            -- smallest sum yet. So, continue searching for cliques.
            let nbrs = pairablePrimes p minSum
                new_tbl = M.insert p (S.fromList nbrs) tbl
             in if length nbrs < groupSize - 1
                    then
                        aux minSum new_tbl ps
                    else case findCliques (reverse nbrs) (groupSize - 1) tbl of
                        [] -> aux minSum new_tbl ps
                        cliques -> aux (min minSum (getMinSumOfClqs p cliques)) new_tbl ps
      where
        getMinSumOfClqs :: Int -> [[Int]] -> Int
        getMinSumOfClqs x clqs = x + minimum (sum <$> clqs)

solve :: String
solve = compute 5
