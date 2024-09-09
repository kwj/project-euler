module Sol.P0056 (compute, solve) where

import Mylib.Math (numOfDigits)
import Mylib.Util (digits)

compute :: String
compute =
    show $ aux 99 99 0
  where
    aux :: Integer -> Int -> Int -> Int
    aux 0 _ answer = answer
    aux a 0 answer = aux (pred a) 99 answer
    aux a b answer =
        let tmp = a ^ b
         in if numOfDigits tmp 10 * 9 < answer
                then aux (pred a) 99 answer
                else aux a (pred b) (max answer (fromIntegral (sum $ digits tmp)))

solve :: String
solve = compute

{-
-- The following is a simple method.

compute :: String
compute =
    show
        . maximum
        $ map (sum . digits) numbers
  where
    numbers =
        [ a ^ b
        | a <- [1 :: Integer .. 99]
        , b <- [1 :: Int .. 99]
        , mod a 10 /= 0
        ]
-}
