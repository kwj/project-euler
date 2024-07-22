module Sol.P0070 (compute, solve) where

import Data.List (sort, uncons, unfoldr)
import Data.Maybe (fromJust)

import Mylib.Math (isqrt)
import Mylib.Prime (prevPrime, primes)
import Mylib.Util (digits, headExn)

import qualified Mylib.Heap.LeftistHeap as H (LeftistHeap, empty, insert, peek)

data Node = Node {n :: Int, r :: Double} deriving (Show)

instance Eq Node where
    (==) :: Node -> Node -> Bool
    (Node _ x) == (Node _ y) = x == y

instance Ord Node where
    compare :: Node -> Node -> Ordering
    compare (Node _ x) (Node _ y) = compare x y

limit :: Int
limit = 9_999_999

prod :: [(Int, Int)] -> Int
prod =
    foldr (\(b, e) acc -> acc * b ^ e) 1

phi :: [(Int, Int)] -> Int
phi =
    foldr (\(b, e) acc -> acc * b ^ (e - 1) * (b - 1)) 1

ratio :: [(Int, Int)] -> Double
ratio pfSeq =
    fromIntegral (prod pfSeq) / fromIntegral (phi pfSeq)

pfSequences :: Int -> Int -> [[(Int, Int)]]
pfSequences x y =
    unfoldr
        ( \pfLst ->
            if length pfLst == 1 && snd (pfLst !! 0) == 1
                then Nothing
                else Just (reverse pfLst, next_pfLst pfLst)
        )
        (if x == y then [(x, 2)] else [(max x y, 1), (min x y, 1)])
  where
    next_pfLst :: [(Int, Int)] -> [(Int, Int)]
    next_pfLst pfSeq
        | e > 1 = (b, e - 1) : (drop 1 pfSeq)
        | otherwise =
            let (tmp_b, tmp_e) = headExn (drop 1 pfSeq)
                prev_p = prevPrime b
             in if prev_p == tmp_b
                    then aux ((tmp_b, tmp_e + 1) : (drop 2 pfSeq))
                    else aux ((prev_p, 1) : (drop 1 pfSeq))
      where
        (b, e) = headExn pfSeq

    aux :: [(Int, Int)] -> [(Int, Int)]
    aux pfSeq =
        if tmp < b
            then pfSeq
            else
                let prev_p = prevPrime (tmp + 1)
                 in if prev_p > b
                        then (prev_p, 1) : pfSeq
                        else (b, e + 1) : (drop 1 pfSeq)
      where
        (b, e) = headExn pfSeq
        tmp = limit `div` (prod pfSeq)

compute :: String
compute =
    show $
        aux
            (H.insert (Node 87109 (87109 / 79180)) H.empty)
            (reverse $ primes 11 (isqrt limit))
  where
    aux :: H.LeftistHeap Node -> [Int] -> Int
    aux pq [] = n (H.peek pq)
    aux pq prime_seq
        | ratio [(p, 1)] > r (H.peek pq) =
            n (H.peek pq)
        | otherwise =
            aux (go pq (pfSequences p (prevPrime ((limit `div` p) + 1)))) ps
      where
        (p, ps) = fromJust $ uncons prime_seq

    go :: H.LeftistHeap Node -> [[(Int, Int)]] -> H.LeftistHeap Node
    go pq [] = pq
    go pq pfseq_seq
        | ratio (take (min 2 (length pfs)) pfs) > r (H.peek pq) =
            pq
        | otherwise =
            let n = prod pfs
                totient = phi pfs
             in if isPermutation n totient
                    then go (H.insert (Node n (fromIntegral n / fromIntegral totient)) pq) pfss
                    else go pq pfss
      where
        (pfs, pfss) = fromJust $ uncons pfseq_seq

    isPermutation :: Int -> Int -> Bool
    isPermutation a b =
        (sort $ digits a) == (sort $ digits b)

solve :: String
solve = compute
