module Sol.P0065 (compute, solve) where

import Data.Ratio (numerator)

import Mylib.Util (digits)

compute :: Int -> String
compute nth =
    show
        . sum
        . digits
        . numerator
        . foldr1 (\x acc -> x + (1 / acc))
        . take nth
        $ (2 :: Rational) : concat [[1, 2 * k, 1] | k <- [1 ..]]

solve :: String
solve = compute 100
