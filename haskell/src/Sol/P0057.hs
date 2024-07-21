module Sol.P0057 (compute, solve) where

import Mylib.Math (numOfDigits)

fractions :: [(Integer, Integer)]
fractions = iterate (\(n, d) -> (2 * d + n, d + n)) (3, 2)

compute :: Int -> String
compute cnt =
    show
        . length
        . filter (\(n, d) -> numOfDigits n 10 > numOfDigits d 10)
        $ take cnt fractions

solve :: String
solve = compute 1_000
