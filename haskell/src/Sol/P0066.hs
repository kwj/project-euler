module Sol.P0066 (compute, solve) where

import Data.Bifunctor (first)
import Data.Function (on)
import Data.List (maximumBy)
import Data.Ratio (numerator, (%))

import Mylib.Math (isqrt)
import Mylib.Util (initExn)

-- Here, I used the 'ScopedTypeVariables' extension enabled in GHC2021.
continuedFraction :: forall int. Integral int => int -> (int, [int])
continuedFraction n
    | isqrt_n * isqrt_n == n =
        (isqrt_n, [])
    | otherwise =
        aux 0 1 isqrt_n []
  where
    isqrt_n = isqrt n
    stop = 2 * isqrt_n

    aux :: int -> int -> int -> [int] -> (int, [int])
    aux b c a lst
        | a == stop =
            (isqrt_n, reverse lst)
        | otherwise =
            let b' = a * c - b
                c' = (n - b' * b') `div` c
                a' = (isqrt_n + b') `div` c'
             in aux b' c' a' (a' : lst)

numerator' :: Integral int => [int] -> int
numerator' lst =
    numerator . foldr1 (\x acc -> x + (1 / acc)) $ (% 1) <$> lst

compute :: Integer -> String
compute limit =
    show
        . snd
        . maximumBy (compare `on` fst)
        . map (first (numerator' . aux))
        . filter (not . null . snd . fst)
        $ zip (continuedFraction <$> [1 .. limit]) [(1 :: Int) ..]
  where
    aux :: Integral int => (int, [int]) -> [int]
    aux (n, lst)
        | even (length lst) = initExn $ n : lst
        | otherwise = initExn $ (n : lst) ++ lst

solve :: String
solve = compute 1_000
