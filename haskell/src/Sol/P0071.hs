module Sol.P0071 (compute, solve) where

{-
Farey sequence
  2/5, 3/7
    -> 2/5, (2+3)/(5+7), 3/7
    -> 2/5, (2+3)/(5+7), (2+3+3)/(5+7+7), 3/7
    -> 2/5, (2+3)/(5+7), (2+3+3)/(5+7+7), (2+3+3+3)/(5+7+7+7), 3/7
     ...
    -> 2/5, ..., (2+3x)/(5+7x), 3/7

      5+7x <= 1_000_000
-}

compute :: Int -> String
compute limit =
    show (2 + 3 * ((limit - 5) `div` 7))

solve :: String
solve = compute 1_000_000
