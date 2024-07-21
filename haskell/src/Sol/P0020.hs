module Sol.P0020 (compute, solve) where

import Mylib.Math (factorial)
import Mylib.Util (digits)

compute :: Integer -> String
compute = show . sum . digits . factorial

solve :: String
solve = compute 100
