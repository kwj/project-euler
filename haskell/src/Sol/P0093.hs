module Sol.P0093 (compute, solve) where

{-
This implementation is little slow. The following is a result on my Raspberry Pi 4.

% cabal v2-run pe-solver -- 93
[Problem 93]
Answer: 1258
Elapsed time: 1.134940 sec.
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
import Data.Maybe (catMaybes, fromJust)
import Data.Ratio (Ratio, denominator, numerator, (%))

import qualified Data.IntSet as S (fromList, member)

import Mylib.Combinatorics (combinations)

fourArithmeticOps ::
    Maybe (Ratio Int) -> Maybe (Ratio Int) -> [Maybe (Ratio Int)]
fourArithmeticOps Nothing _ = [Nothing]
fourArithmeticOps _ Nothing = [Nothing]
fourArithmeticOps (Just x) (Just y) = do
    op_number <- [1 .. 6]
    pure (aux op_number x y)
  where
    aux :: Int -> Ratio Int -> Ratio Int -> Maybe (Ratio Int)
    aux 1 a b = Just (a + b)
    aux 2 a b = Just (a - b)
    aux 3 a b = Just (b - a)
    aux 4 a b = Just (a * b)
    aux 5 _ 0 = Nothing
    aux 5 a b = Just (a / b)
    aux 6 0 _ = Nothing
    aux 6 a b = Just (b / a)
    aux _ _ _ = error "fatal error (unreachable)"

makeNumbers :: [Maybe (Ratio Int)] -> [Int]
makeNumbers xs =
    map numerator
        . filter (\x -> denominator x == 1)
        . catMaybes
        $ aux xs
  where
    aux :: [Maybe (Ratio Int)] -> [Maybe (Ratio Int)]
    aux lst =
        (\x -> case_1 x ++ case_2 x) =<< choiceTwo lst

    choiceTwo :: Eq a => [a] -> [([a], [a])]
    choiceTwo lst =
        (id &&& (lst \\)) <$> combinations 2 lst

case_1 :: ([Maybe (Ratio Int)], [Maybe (Ratio Int)]) -> [Maybe (Ratio Int)]
case_1 (lst1, lst2) =
    let l1 = fourArithmeticOps d1 d2 >>= fourArithmeticOps d3 >>= fourArithmeticOps d4
        l2 = fourArithmeticOps d1 d2 >>= fourArithmeticOps d4 >>= fourArithmeticOps d3
     in l1 ++ l2
  where
    d1 = lst1 !! 0
    d2 = lst1 !! 1
    d3 = lst2 !! 0
    d4 = lst2 !! 1

case_2 :: ([Maybe (Ratio Int)], [Maybe (Ratio Int)]) -> [Maybe (Ratio Int)]
case_2 (lst1, lst2) =
    concat $
        liftA2 fourArithmeticOps (fourArithmeticOps d1 d2) (fourArithmeticOps d3 d4)
  where
    d1 = lst1 !! 0
    d2 = lst1 !! 1
    d3 = lst2 !! 0
    d4 = lst2 !! 1

countConsecNumbers :: [Maybe (Ratio Int)] -> Int
countConsecNumbers lst =
    go 1
  where
    set = S.fromList (makeNumbers lst)

    go :: Int -> Int
    go cnt
        | S.member cnt set == True =
            go (succ cnt)
        | otherwise =
            pred cnt

compute :: String
compute =
    concatMap (show . numerator . fromJust)
        . snd
        . maximumBy (compare `on` fst)
        . map (countConsecNumbers &&& id)
        $ combinations 4 numbers
  where
    numbers = (\n -> Just (n % 1)) <$> [1 .. 9]

solve :: String
solve = compute

{-
The following implementation is about twice slower than the above. I wonder why.

compute :: String
compute =
    concatMap (show . numerator . fromJust)
        . maximumBy (compare `on` countConsecNumbers)
        $ combinations 4 numbers

% cabal v2-run pe-solver -- 93
[Problem 93]
Answer: 1258
Elapsed time: 2.213767 sec.
-}
