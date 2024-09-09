module Sol.P0093 (compute, solve) where

{-
This implementation is little slow. The following is a result on my Raspberry Pi 4.

% cabal v2-run pe-solver -- 93
[Problem 93]
Answer: 1258
Elapsed time: 0.843592 sec.
-}

{-
arithmetic operations (fourArithmeticOps):
  commutative:
    addition: X + Y
    multiplication: X * Y
  no-commutative:
    subtraction:  X - Y, Y - X
    division: X / Y, Y / X

patterns:
  d1, d2, d3, d4: numbers

  [case 1]
     ((d1 OP d2) OP d3) OP d4
      ^^^----^^^
     ^^^^^^^^^^^----^^^
     ^^^^^^^^^^^^^^^^^^----^^

     ((d1 OP d2) OP d4) OP d3
      ^^^----^^^
     ^^^^^^^^^^^----^^^
     ^^^^^^^^^^^^^^^^^^----^^
  [case 2]
     (d1 OP d2) OP (d3 OP d4)
     ^^^----^^^    ^^^----^^^
     ^^^^^^^^^^----^^^^^^^^^^

  ^^-^^: We can ignore the order of the two terms because
         fourArithmeticOps() considers no-commutative operations.
-}

import Control.Arrow ((&&&))
import Data.Function (on)
import Data.List (maximumBy, (\\))
import Data.Ratio (Ratio, denominator, numerator)

import qualified Data.IntSet as S (fromList, member)

import Mylib.Combinatorics (combinations)
import Mylib.Util (headExn, tailExn)

fourArithmeticOps :: Ratio Int -> Ratio Int -> [Ratio Int]
fourArithmeticOps x y
    | x == 0 =
        [negate y, 0, y]
    | y == 0 =
        [negate x, 0, x]
    | otherwise =
        [x + y, x - y, y - x, x * y, x / y, y / x]

{- HLINT ignore case_1 "Use head" -}
case_1 :: ([Ratio Int], [Ratio Int]) -> [Ratio Int]
case_1 (lst1, lst2) =
    let res1 = d1d2 >>= fourArithmeticOps d3 >>= fourArithmeticOps d4
        res2 = d1d2 >>= fourArithmeticOps d4 >>= fourArithmeticOps d3
     in res1 ++ res2
  where
    d1d2 = fourArithmeticOps (lst1 !! 0) (lst1 !! 1)
    d3 = lst2 !! 0
    d4 = lst2 !! 1

{- HLINT ignore case_2 "Use head" -}
case_2 :: ([Ratio Int], [Ratio Int]) -> [Ratio Int]
case_2 (lst1, lst2) =
    concat $ liftA2 fourArithmeticOps d1d2 d3d4
  where
    d1d2 = fourArithmeticOps (lst1 !! 0) (lst1 !! 1)
    d3d4 = fourArithmeticOps (lst2 !! 0) (lst2 !! 1)

makeIntNumbers :: [Ratio Int] -> [Int]
makeIntNumbers =
    map numerator . filter ((== 1) . denominator) . aux
  where
    aux :: [Ratio Int] -> [Ratio Int]
    aux lst =
        l1 ++ l2
      where
        l1 = case_1 . (id &&& (lst \\)) =<< combinations 2 lst
        l2 = case_2 . (id &&& (lst \\)) =<< map (: [headExn lst]) (tailExn lst)

countConsecNumbers :: [Ratio Int] -> Int
countConsecNumbers lst =
    go 1
  where
    set = S.fromList (makeIntNumbers lst)

    go :: Int -> Int
    go cnt
        | S.member cnt set =
            go (succ cnt)
        | otherwise =
            pred cnt

compute :: String
compute =
    concatMap (show . numerator)
        . snd
        . maximumBy (compare `on` fst)
        . map (countConsecNumbers &&& id)
        $ combinations 4 [1 .. 9 :: Ratio Int]

solve :: String
solve = compute

{-
The following implementation is about twice slower than the above. I wonder why.

compute :: String
compute =
    concatMap (show . numerator)
        . maximumBy (compare `on` countConsecNumbers)
        $ combinations 4 [1 .. 9 :: Ratio Int]

% cabal v2-run pe-solver -- 93
[Problem 93]
Answer: 1258
Elapsed time: 1.699603 sec.
-}
