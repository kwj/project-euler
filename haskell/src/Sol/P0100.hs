module Sol.P0100 (compute, solve) where

import Mylib.Util (headExn)

peSols :: [(Int, Int)]
peSols = iterate (\(x, y) -> (3 * x + 4 * y, 2 * x + 3 * y)) (1, 1)

compute :: Int -> String
compute limit =
    show $ (y + 1) `div` 2 -- a = (y + 1) / 2
  where
    border = 2 * limit - 1 -- x = 2b - 1
    y = snd . headExn $ dropWhile (\(x, _) -> x <= border) peSols

solve :: String
solve = compute 1_000_000_000_000
