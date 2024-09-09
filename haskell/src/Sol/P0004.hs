module Sol.P0004 (compute, solve) where

import Control.Arrow ((&&&))
import Data.Foldable (asum)

import Mylib.Math (isPalindrome)

compute :: Int -> String
compute n
    | n < 1 = error "range error"
    | otherwise =
        case asum $ maxPalindromeNumber <$> blocks of
            Just x -> show x
            _ -> "not found"
  where
    nUpper = 10 ^ n - 1
    nLower = 10 ^ (n - 1)
    blkUpperLimit = 10 ^ (n * 2)
    blkLowerLimit = if n == 1 then 0 else 10 ^ ((n - 1) * 2)
    blkSize = 10 ^ (n * 2 - 2)

    -- descending list of block ranges [(lower, uppper), ...]
    blocks :: [(Int, Int)]
    blocks =
        map (subtract blkSize &&& subtract 1)
            . takeWhile (> blkLowerLimit)
            $ iterate (subtract blkSize) blkUpperLimit

    -- find the maximum palindrome number in the block
    maxPalindromeNumber :: (Int, Int) -> Maybe Int
    maxPalindromeNumber block
        | null numbers = Nothing
        | otherwise = Just (maximum numbers)
      where
        numbers = findPalindromeNumbers block

        findPalindromeNumbers :: (Int, Int) -> [Int]
        findPalindromeNumbers (blkLower, blkUpper) =
            concatMap
                ( filter (`isPalindrome` 10)
                    . (filter (>= blkLower) . (\x -> (x *) <$> [nLower .. (min x (blkUpper `div` x))]))
                )
                $ dropWhile (\x -> x * x < blkLower) [nLower .. nUpper]

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
