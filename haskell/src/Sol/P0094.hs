module Sol.P0094 (compute, solve) where

import Mylib.Util (headExn, tailExn)

-- Pell's equation: X^2 - 3Y^2 = 1
solutionPairs :: [(Int, Int)]
solutionPairs =
    drop 1 $ iterate (\(x, y) -> (2 * x + 3 * y, x + 2 * y)) (2, 1)

compute :: Int -> String
compute limit =
    show $ go solutionPairs 0
  where
    go :: [(Int, Int)] -> Int -> Int
    go pairs acc
        | p > limit =
            acc
        | otherwise =
            go (tailExn pairs) (acc + p)
      where
        (a, _) = headExn pairs
        p = if mod a 3 == 2 then 2 * a - 2 else 2 * a + 2

solve :: String
solve = compute 1_000_000_000
