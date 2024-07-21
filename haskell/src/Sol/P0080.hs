module Sol.P0080 (compute, solve) where

import Mylib.Math (isqrt)
import Mylib.Util (digits)

compute :: Integer -> Int -> String
compute limit ndigit =
    show $
        sum
            [ sum . take ndigit . reverse $ digits x
            | n <- [1 .. limit]
            , isqrt n * isqrt n /= n
            , let x = isqrt (pow10 * n)
            ]
  where
    pow10 = 10 ^ ((ndigit - 1) * 2) :: Integer

solve :: String
solve = compute 100 100
