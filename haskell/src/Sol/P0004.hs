module Sol.P0004 (compute, solve) where

import Mylib.Math (isPalindrome)

compute :: Int -> String
compute n
    | n < 1 = error "range error"
    | otherwise =
        case maxPalindrome blocks of
            Just x -> show (x :: Int)
            Nothing -> error "not found"
  where
    nUpper = 10 ^ n - 1
    nLower = 10 ^ (n - 1)
    blkUpperLimit = 10 ^ (n * 2)
    blkLowerLimit = if n == 1 then 0 else 10 ^ ((n - 1) * 2)
    blkSize = 10 ^ (n * 2 - 2)
    blocks =
        map (\x -> (x - 1, x - blkSize)) $
            takeWhile (> blkLowerLimit) $
                iterate (\x -> x - blkSize) blkUpperLimit
    aux blkUpper blkLower =
        filter (\x -> isPalindrome x 10) $
            filter (>= blkLower) $
                concat $
                    map (\x -> map (\y -> x * y) [nLower .. (min x (blkUpper `div` x))]) $
                        filter (\x -> x * x >= blkLower) [nLower .. nUpper]
    maxPalindrome (tpl : tpls) =
        let lst = aux (fst tpl) (snd tpl)
         in if null lst then maxPalindrome tpls else Just (maximum lst)
    maxPalindrome [] = Nothing

solve :: String
solve = compute 3

{-
-- The following is a simple method, though less efficient.

compute :: Int -> String
compute n =
    show $
        maximum $
            filter (\z -> isPalindrome z 10) [x * y | x <- [nLower .. nUpper], y <- [nLower .. nUpper]]
  where
    nUpper = 10 ^ n - 1 :: Int
    nLower = 10 ^ (n - 1) :: Int
-}
