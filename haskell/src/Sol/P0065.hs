module Sol.P0065 (compute, solve) where

{-
  e = [2; 1, 2, 1, 1, 4, 1, 1, 6, ..., 1, 1, 2k, ...]
      [a{0}; a{1}, a{2}, ...]
-}

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
