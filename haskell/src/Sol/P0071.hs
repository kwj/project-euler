module Sol.P0071 (compute, solve) where

compute :: Int -> String
compute limit =
    show (2 + 3 * ((limit - 5) `div` 7))

solve :: String
solve = compute 1_000_000
