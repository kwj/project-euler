module Sol.P0004 (compute, solve) where

import Control.Arrow ((&&&))
import Data.Foldable (asum)

import Mylib.Math (isPalindrome, isqrt)

compute :: Int -> String
compute n
    | n < 1 = error "range error"
    | otherwise =
        case asum $ maxPalindromeNumber <$> blocks of
            Just x -> show x
            _ -> "not found"
  where
    nUpper = 10 ^ n - 1
    nLower = if n == 1 then 0 else 10 ^ (n - 1)
    blkSize = 10 ^ (n * 2 - 2)

    -- descending list of block ranges [(lower, uppper), ...]
    blocks :: [(Int, Int)]
    blocks =
        map (id &&& (+ (blkSize - 1)))
            . takeWhile (>= nLower * nLower)
            $ iterate (subtract blkSize) (((nUpper * nUpper) `div` blkSize) * blkSize)

    -- find the maximum palindrome number in the block
    maxPalindromeNumber :: (Int, Int) -> Maybe Int
    maxPalindromeNumber block
        | null numbers = Nothing
        | otherwise = Just (maximum numbers)
      where
        numbers = findPalindromeNumbers block

        findPalindromeNumbers :: (Int, Int) -> [Int]
        findPalindromeNumbers (blkLower, blkUpper) =
            [ prod
            | x <- [isqrt blkLower .. nUpper]
            , y <- [nLower .. if x == 0 then 0 else min x (blkUpper `div` x)]
            , let prod = x * y
            , prod >= blkLower
            , prod `isPalindrome` 10
            ]

solve :: String
solve = compute 3

{-
-- The following is a simple method, though less efficient.

compute :: Int -> String
compute n =
    show
        . maximum
        $ filter (flip isPalindrome 10) [x * y | x <- [nLower .. nUpper], y <- [nLower .. nUpper]]
  where
    nUpper = 10 ^ n - 1 :: Int
    nLower = 10 ^ (n - 1) :: Int
-}
