module Sol.P0093 (compute, solve) where

{-
arithmetic operations (fourArithmeticOps):
  commutative:
    addition: X + Y
    multiplication: X * Y
  no-commutative:
    subtraction:  X - Y, Y - X
    division: X / Y, Y / X

patterns:
  A, B, C, D: numbers

  [case 1]
     ((A op B) op C) op D
      ^^^--^^^
     ^^^^^^^^^^--^^^
     ^^^^^^^^^^^^^^^^--^^
  [case 2]
     (A op B) op (C op D)
     ^^^--^^^    ^^^--^^^
     ^^^^^^^^^--^^^^^^^^^

  ^^-^^: We can ignore the order of the two terms because
         fourArithmeticOps() considers no-commutative operations.
-}

import Data.Function (on)
import Data.List (maximumBy, nub, sort, (\\))
import Data.Maybe (fromJust)
import Data.Ratio (Ratio, denominator, numerator, (%))

import qualified Data.Set as S (fromList, member)

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
    nub
        . sort
        . map (numerator)
        . filter (\x -> denominator x == 1)
        . map (fromJust)
        . filter (/= Nothing)
        $ aux xs
  where
    aux :: [Maybe (Ratio Int)] -> [Maybe (Ratio Int)]
    aux lst = do
        pair_lst <- choiceTwo lst
        case_1 pair_lst ++ case_2 pair_lst

    choiceTwo :: Eq a => [a] -> [([a], [a])]
    choiceTwo lst = do
        two <- combinations 2 lst
        pure (two, lst \\ two)

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
    concat $ fourArithmeticOps <$> fourArithmeticOps d1 d2 <*> fourArithmeticOps d3 d4
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
        . map (\lst -> (countConsecNumbers lst, lst))
        $ combinations 4 numbers
  where
    numbers = map (\n -> Just (n % 1)) [1 .. 9]

solve :: String
solve = compute
