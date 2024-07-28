module Sol.P0027 (compute, solve) where

{-
n^2 + an + b,  where abs(a) < 1000 and abs(b) < 1000

when 'n' = 0:
  '0 + 0 + b' = 'b' must be a prime number. so, 2 < 'b' < 1000.
      'b' must not be 2
      if 'n' is an even number, the value of the expression becomes even
when 'n' = 1:
  '1 + a + b' must be a prime number.
  write this prime number is 'x', then 'a' = 'x' - 'b' - 1.
      abs('x' - b - 1) < 1000 and 2 < 'b' < 1000 ===> 0 < 'x' < 2000
when 'n' is a odd number:
  'n^2 + b' is a even number. so 'a' must be a odd number.
-}

import Data.Function (on)
import Data.List (maximumBy)

import Mylib.Prime (isPrime, primeNumbers)

consecLength :: Int -> Int -> Int
consecLength a b =
    aux 1
  where
    aux n
        | isPrime (n * n + a * n + b) = aux (succ n)
        | otherwise = n

compute :: String
compute =
    let (a, b) = snd $ maximumBy (compare `on` fst) lst
     in show (a * b)
  where
    plst = takeWhile (< 2000) primeNumbers
    lst =
        [ (consecLength a b, (a, b))
        | b <- takeWhile (<= 1000) (drop 1 plst)
        , a <- (\x -> x - b - 1) <$> plst
        , abs a < 1000
        ]

solve :: String
solve = compute
