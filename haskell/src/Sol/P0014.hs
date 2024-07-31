module Sol.P0014 (compute, solve) where

import Control.Arrow ((&&&))
import Data.Function (on)
import Data.List (maximumBy)

-- It is a straightforward method that doesn't use memoization.
getCollatzLength :: Int -> Int
getCollatzLength n = aux n 1
  where
    aux :: Int -> Int -> Int
    aux 1 cnt = cnt
    aux x cnt
        | even x = aux (x `div` 2) (succ cnt)
        | otherwise = aux (3 * x + 1) (succ cnt)

compute :: Int -> String
compute limit =
    show
        . snd
        . maximumBy (compare `on` fst)
        $ (getCollatzLength &&& id) <$> [(div limit 2) .. (limit - 1)]

solve :: String
solve = compute 1_000_000
