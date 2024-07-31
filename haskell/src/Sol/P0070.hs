module Sol.P0070 (compute, solve) where

{-
  The answer must be a composite number because prime 'n' is not a permutation of phi(n) = n - 1.

  n = p1^k1 * p2^k2 * p3^k3 * ... * p{r}^k{n}
  -->
    phi(N) = N * (1-1/p1) * (1-1/p2) * (1-1/p3) * ... * (1-1/p{n})
      <-->
    N/phi(N) = (p1/(p1-1)) * (p2/(p2-1)) * (p3/(p3-1)) * ... * (p{n}/(p{n}-1))

  From the problem statement, 87109/phi(87109) = 87109 / 79180 = 1.1001.
  11/10 = 1.1 and 7/6 = 1.666..., so 11 <= prime numbers <= 9_999_999 / 11 = 909090.8181...

  The answer N has the following form (p_i are prime numbers)

    N = p1^k1 * p2^k2 * ... * pn^kn  (N < 10^7, n > 1, 11 <= p1 < p2 < ... < pn, k1>2 when n=1)
-}

import Data.List (sort, unfoldr)

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
    aux pq (p : ps)
        | ratio [(p, 1)] > r (H.peek pq) =
            n (H.peek pq)
        | otherwise =
            aux (go pq (pfSequences p (prevPrime ((limit `div` p) + 1)))) ps

    go :: H.LeftistHeap Node -> [[(Int, Int)]] -> H.LeftistHeap Node
    go pq [] = pq
    go pq (pfs : pfss)
        | ratio (take (min 2 (length pfs)) pfs) > r (H.peek pq) =
            pq
        | otherwise =
            let n = prod pfs
                totient = phi pfs
             in if isPermutation n totient
                    then go (H.insert (Node n (fromIntegral n / fromIntegral totient)) pq) pfss
                    else go pq pfss

    isPermutation :: Int -> Int -> Bool
    isPermutation a b =
        (sort $ digits a) == (sort $ digits b)

solve :: String
solve = compute
