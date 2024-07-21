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

import Data.Array.ST (MArray, newArray, readArray, runSTUArray, writeArray)
import Data.Array.Unboxed (UArray)
import Data.Foldable (for_)
import Data.List (group, sort, uncons)

import qualified Data.Numbers.Primes as P (primeFactors, primes)

import Mylib.Util (tailExn)

import qualified Mylib.MinFactor as MF (minFactor, minFactorTbl)

primeFactorization :: Int -> [(Int, Int)]
primeFactorization =
    map (\x -> (maybe (error "") fst (uncons x), length x)) . group . P.primeFactors

pfactorsToDivisors :: [Int] -> [Int]
pfactorsToDivisors [] = []
pfactorsToDivisors lst =
    pfactorsToDivisors' (fmap (scanl1 (*)) $ group . sort $ lst) [1]

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
    for_ primes $ \p -> do
        let qs = takeWhile (<= limit) $ scanl1 (*) $ repeat p
            xs = tailExn $ scanl (\acc q -> acc + q ^ z) 0 qs
         in for_ (zipWith (\q x -> (q, x)) qs xs) $ \(q, x) -> do
                tmp <- readArray tbl q
                writeArray tbl q (tmp + x)
    for_ primes $ \p -> do
        for_ (takeWhile (<= limit) $ scanl1 (*) $ repeat p) $ \q -> do
            for_ [2 .. (limit `div` q)] $ \n -> do
                tmp_n <- readArray tbl n
                tmp_q <- readArray tbl q
                if tmp_n == 1 || n `mod` p == 0
                    then pure ()
                    else writeArray tbl (q * n) (tmp_q * tmp_n)
    writeArray tbl 0 0
    return tbl
  where
    primes = takeWhile (<= limit) P.primes

aliquotSumTbl :: Int -> UArray Int Int
aliquotSumTbl limit =
    runSTUArray $ do
        tbl <- sigmaTbl' 1 limit
        for_ [1 .. limit] $ \x -> do
            tmp <- readArray tbl x
            writeArray tbl x (tmp - x)
        return tbl
