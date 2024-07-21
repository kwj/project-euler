module Sol.P0036 (compute, solve) where

import Mylib.Math (isPalindrome)

compute :: Int -> String
compute limit =
    show
        . sum
        $ filter (\x -> isPalindrome x 10 && isPalindrome x 2) [1, 3 .. limit]

solve :: String
solve = compute 1_000_000
