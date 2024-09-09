module Sol.P0055 (compute, solve) where

import Mylib.Util (digits, undigits)

reverseNumber :: Integer -> Integer
reverseNumber = undigits . reverse . digits

isLychrel :: Integer -> Bool
isLychrel n =
    aux n (reverseNumber n) 50
  where
    aux :: Integer -> Integer -> Int -> Bool
    aux _ _ 0 = True
    aux n1 n2 cnt =
        let next_n = n1 + n2
            tmp = reverseNumber next_n
         in ((next_n /= tmp) && aux next_n tmp (pred cnt))

compute :: Integer -> String
compute limit =
    show . length $ filter isLychrel [1 .. limit]

solve :: String
solve = compute 10_000
