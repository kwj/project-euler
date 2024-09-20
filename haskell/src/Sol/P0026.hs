module Sol.P0026 (compute, solve) where

import Mylib.Factor (divisors, primeFactorization)
import Mylib.Math (powerModExn)
import Mylib.Util (headExn)

pp :: Int -> Int
pp = iterDiv 5 . iterDiv 2
  where
    iterDiv :: Int -> Int -> Int
    iterDiv divisor x
        | x `mod` divisor == 0 = iterDiv divisor (x `div` divisor)
        | otherwise = x

-- This implementation isn't a correct Carmichael function because
-- it assumes that the argument is odd (not a multiple of 2).
carmichael :: Int -> Int
carmichael n =
    foldl1 lcm
        . map (\(x0, x1) -> (x0 - 1) * (x0 ^ (x1 - 1)))
        $ primeFactorization n

findRepetendLength :: Int -> Int
findRepetendLength n
    | d == 1 =
        0
    | otherwise =
        headExn
            . filter ((== 1) . (flip . powerModExn) 10 d)
            . divisors
            $ carmichael d
  where
    d = pp n

compute :: Int -> String
compute limit =
    show $ aux (reverse [(div limit 2) .. (limit - 1)]) 0 0
  where
    aux :: [Int] -> Int -> Int -> Int
    aux [] _ answer = answer
    aux (x : xs) max_length answer
        | x <= max_length =
            answer
        | otherwise =
            let tmp = findRepetendLength x
             in if tmp > max_length
                    then
                        aux xs tmp (pp x)
                    else
                        aux xs max_length answer

solve :: String
solve = compute 1_000
