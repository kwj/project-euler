module Sol.P0027 (compute, solve) where

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
    let (a, b) = snd $ maximum pairs
     in show (a * b)
  where
    plst = takeWhile (< 2000) primeNumbers
    pairs =
        [ (consecLength a b, (a, b))
        | b <- takeWhile (<= 1000) (drop 1 plst)
        , a <- map (\x -> x - b - 1) plst
        , abs a < 1000
        ]

solve :: String
solve = compute
