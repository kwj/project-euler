module Sol.P0025 (compute, solve) where

import Data.List (unfoldr)

import Mylib.Util (headExn)

compute :: Int -> String
compute ndigits =
    show
        . fst
        . headExn
        . dropWhile ((< 10 ^ (pred ndigits)) . snd)
        $ zip [1 :: Int ..] fibs
  where
    fibs :: [Integer]
    fibs = unfoldr (\(x, y) -> Just (x, (y, x + y))) (1, 1)

solve :: String
solve = compute 1_000
