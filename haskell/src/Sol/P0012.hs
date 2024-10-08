module Sol.P0012 (compute, solve) where

{-
triangle number's formula is n(n + 1)/2 and 'n' and 'n + 1' are coprime.
Therefore, ...
  - 'n/2' and 'n+1' are coprime (when 'n' is even)
  - 'n' and '(n+1)/2' are coprime (when 'n' is odd)

assume that f(n) returns number of divisors of 'n'.
f(a*b) = f(a) * f(b) when 'a' and 'b' are coprime.
-}

import Mylib.Factor (divisors)
import Mylib.Util (headExn)

numberOfDivisors :: Int -> Int
numberOfDivisors x
    | odd x = ndivs x * ndivs (div (x + 1) 2)
    | otherwise = ndivs (div x 2) * ndivs (x + 1)
  where
    ndivs = length . divisors

compute :: Int -> String
compute thr =
    show $ nth * (nth + 1) `div` 2
  where
    nth = headExn $ dropWhile ((<= thr) . numberOfDivisors) [1 ..]

solve :: String
solve = compute 500
