module Sol.P0057 (compute, solve) where

{-
use recurrence relation:
   sqrt(2) = 1 + sqrt(2) - 1
           = 1 + 1 / ( 1 / (sqrt(2) - 1) )
           = 1 + 1 / ( (sqrt(2) + 1) / (2 - 1) )
           = 1 + 1 / (1 + sqrt(2))
   -->
   a{1} = 1 + 1/2
   a{n} = 1 + 1/(1 + a{n-1})    [n>1]

assume that b{n}/c{n} = a{n}
   b{1}/c{1} = 1 + 1/2 = 3/2
   b{n}/c{n} = 1 + 1/(1 + b{n-1}/c{n-1})
             = 1 + 1/((c{n-1) + b{n-1})/c{n-1})
             = 1 + c{n-1}/(c{n-1) + b{n-1})
             = (c{n-1) + b{n-1} + c{n-1))/(c{n-1) + b{n-1})
             = (2 * c{n-1} + b{n-1}) / (c{n-1) + b{n-1})
-}

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
