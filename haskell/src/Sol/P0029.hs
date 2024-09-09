module Sol.P0029 (compute, solve) where

import Data.Array.ST (newArray, runSTUArray, writeArray)
import Data.Array.Unboxed (UArray, elems, (!))
import Data.Foldable (for_)

import qualified Data.IntSet as S (IntSet, empty, fromList, member, union)

import Mylib.Math (isqrt, maxPower)

dupTbl :: Int -> UArray Int Int
dupTbl upper = runSTUArray $ do
    tbl <- newArray (2, max_power) 0
    for_ [2 .. max_power] (\x -> writeArray tbl x (sum $ dupList x upper))
    pure tbl
  where
    max_power = maxPower upper 2

dupList :: Int -> Int -> [Int]
dupList x upper =
    elems dups
  where
    dups :: UArray Int Int = runSTUArray $ do
        tbl <- newArray (2, upper) 0
        for_ [1 .. (x - 1)] $ \y -> do
            let k = lcm x y `div` x
            for_
                [(max k 2), max k 2 + k .. ((upper * y) `div` x)]
                (\idx -> writeArray tbl idx 1)
        pure tbl

compute :: Int -> String
compute upper =
    show $ aux [2 .. base_limit] S.empty ((upper - 1) * (upper - 1))
  where
    base_limit = isqrt upper
    tbl = dupTbl upper

    aux :: [Int] -> S.IntSet -> Int -> Int
    aux [] _ answer = answer
    aux (b : bs) skips answer
        | S.member b skips = aux bs skips answer
        | otherwise =
            let es = [2 .. (maxPower upper b)]
                dup_count = sum $ (tbl !) <$> es
                add_skips = S.fromList . filter (<= base_limit) $ (b ^) <$> es
             in aux bs (S.union skips add_skips) (answer - dup_count)

solve :: String
solve = compute 100

{-
-- The following is a simple and quick method, though less efficient.
import Data.List (group, sort)

compute :: Integer -> String
compute upper =
    show $ length $ group $ sort [a ^ b | a <- [2 .. upper], b <- [2 .. upper]]

solve :: String
solve = compute 100
-}
