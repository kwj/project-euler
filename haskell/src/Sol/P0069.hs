module Sol.P0069 (compute, solve) where

import Mylib.Prime (primeNumbers)
import Mylib.Util (lastExn)

compute :: Int -> String
compute limit =
    show
        . lastExn
        . takeWhile (<= limit)
        $ scanl1 (*) primeNumbers

solve :: String
solve = compute 1_000_000
