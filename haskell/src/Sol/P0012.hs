module Sol.P0012 (compute, solve) where

import Mylib.Factor (divisors)
import Mylib.Util (headExn)

numberOfDivisors :: Int -> Int
numberOfDivisors nth
    | odd nth = (ndivs nth) * (ndivs (div (nth + 1) 2))
    | otherwise = (ndivs (div nth 2)) * (ndivs (nth + 1))
  where
    ndivs = length . divisors

compute :: Int -> String
compute thr = show $ n'th * (n'th + 1) `div` 2
  where
    n'th = headExn $ dropWhile (\x -> numberOfDivisors x <= thr) [1 ..]

solve :: String
solve = compute 500
