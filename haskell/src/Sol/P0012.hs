module Sol.P0012 (compute, solve) where

import Mylib.Factor (divisors)
import Mylib.Util (headExn)

numberOfDivisors :: Int -> Int
numberOfDivisors x
    | odd x = (ndivs x) * (ndivs (div (x + 1) 2))
    | otherwise = (ndivs (div x 2)) * (ndivs (x + 1))
  where
    ndivs = length . divisors

compute :: Int -> String
compute thr =
    show $ nth * (nth + 1) `div` 2
  where
    nth = headExn $ dropWhile (\x -> numberOfDivisors x <= thr) [1 ..]

solve :: String
solve = compute 500
