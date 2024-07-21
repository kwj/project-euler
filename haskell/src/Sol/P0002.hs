module Sol.P0002 (compute, solve) where

import Data.List (unfoldr)

import Mylib.Util (headExn, lastExn)

evenFibs :: [Int]
evenFibs = unfoldr (\(x, y) -> Just (x, (y, 4 * y + x))) (2, 8)

compute :: Int -> String
compute limit =
    show $ div (x + y - 2) 4
  where
    (lst1, lst2) = break (> limit) evenFibs
    x = lastExn lst1
    y = headExn lst2

solve :: String
solve = compute 4_000_000

{-
-- The following is a simple method.

compute :: Int -> String
compute limit = show $ sum $ takeWhile (< limit) evenFibs
-}
