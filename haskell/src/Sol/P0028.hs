module Sol.P0028 (compute, solve) where

compute :: Int -> String
compute sideLen =
    show
        . foldr (+) 1
        $ map
            (\n -> 16 * n * n + 4 * n + 4)
            [1 .. (div (sideLen - 1) 2)]

solve :: String
solve = compute 1_001
