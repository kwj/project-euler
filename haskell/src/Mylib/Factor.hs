module Mylib.Factor (
    primeFactorization,
    pfactorsToDivisors,
    divisors,
    sigmaTbl,
    aliquotSumTbl,
    P.primeFactors,
    MF.minFactor,
    MF.minFactorTbl,
) where

import Control.Arrow ((&&&))
import Control.Monad (when)
import Data.Array.ST (
    MArray,
    modifyArray,
    newArray,
    readArray,
    runSTUArray,
    writeArray,
 )
import Data.Array.Unboxed (UArray)
import Data.Foldable (for_)
import Data.List (group, sort, uncons)
import Data.Maybe (fromJust)

import qualified Data.Numbers.Primes as P (primeFactors, primes)

import Mylib.Util (tailExn)

import qualified Mylib.MinFactor as MF (minFactor, minFactorTbl)

primeFactorization :: Int -> [(Int, Int)]
primeFactorization =
    map (fst . fromJust . uncons &&& length) . group . P.primeFactors

pfactorsToDivisors :: [Int] -> [Int]
pfactorsToDivisors [] = []
pfactorsToDivisors lst =
    pfactorsToDivisors' (map (scanl1 (*)) . group $ sort lst) [1]

pfactorsToDivisors' :: [[Int]] -> [Int] -> [Int]
pfactorsToDivisors' lst divs =
    foldl
        (\result xs -> result ++ map product (sequence [result, xs]))
        divs
        lst

divisors :: Int -> [Int]
divisors n
    | n <= 0 = error "invalid range"
    | n == 1 = [1]
    | otherwise = pfactorsToDivisors $ P.primeFactors n

-- https://en.wikipedia.org/wiki/Divisor_function
sigmaTbl :: Int -> Int -> UArray Int Int
sigmaTbl z limit =
    runSTUArray (sigmaTbl' z limit)

sigmaTbl' :: MArray a Int m => Int -> Int -> m (a Int Int)
sigmaTbl' z limit = do
    tbl <- newArray (0, limit) 1
    for_ primes $ \p ->
        let qs = takeWhile (<= limit) . scanl1 (*) $ repeat p
            xs = tailExn $ scanl (\acc q -> acc + q ^ z) 0 qs
         in for_ (zip qs xs) (\(q, x) -> modifyArray tbl q (+ x))
    for_ primes $ \p ->
        for_ (takeWhile (<= limit) . scanl1 (*) $ repeat p) $ \q ->
            for_ [2 .. (limit `div` q)] $ \n -> do
                tmp_n <- readArray tbl n
                tmp_q <- readArray tbl q
                when (tmp_n /= 1 && n `mod` p /= 0) (writeArray tbl (q * n) (tmp_q * tmp_n))
    writeArray tbl 0 0
    pure tbl
  where
    primes = takeWhile (<= limit) P.primes

aliquotSumTbl :: Int -> UArray Int Int
aliquotSumTbl limit =
    runSTUArray $ do
        tbl <- sigmaTbl' 1 limit
        for_ [1 .. limit] (\x -> modifyArray tbl x (subtract x))
        pure tbl
