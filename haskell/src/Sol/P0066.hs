module Sol.P0066 (compute, solve) where

import Data.Function (on)
import Data.List (maximumBy)
import Data.Ratio (numerator, (%))

import Mylib.Math (isqrt)
import Mylib.Util (initExn)

continuedFraction :: Integral int => int -> (int, [int])
continuedFraction n =
    aux 0 1 isqrt_n []
  where
    isqrt_n = isqrt n
    stop = 2 * isqrt_n
    aux b c a lst
        | isqrt_n * isqrt_n == n =
            (isqrt_n, [])
        | a == stop =
            (isqrt_n, reverse lst)
        | otherwise =
            let b' = a * c - b
                c' = (n - b' * b') `div` c
                a' = (isqrt_n + b') `div` c'
             in aux b' c' a' (a' : lst)

numerator' :: Integral int => [int] -> int
numerator' lst =
    numerator . foldr1 (\x acc -> x + (1 / acc)) $ map (\x -> x % 1) lst

compute :: Integer -> String
compute limit =
    show
        . snd
        . maximumBy (compare `on` fst)
        . map (\(lst, n) -> ((numerator' lst), n))
        . map (\tpl -> ((aux $ fst tpl), snd tpl))
        . filter (not . null . snd . fst)
        $ zipWith
            (\cf n -> (cf, n))
            (map (continuedFraction) [1 .. limit])
            [(1 :: Int) ..]
  where
    aux :: Integral int => (int, [int]) -> [int]
    aux tpl =
        if even $ length lst
            then initExn $ n : lst
            else initExn $ (n : lst) ++ lst
      where
        n = fst tpl
        lst = snd tpl

solve :: String
solve = compute 1_000
