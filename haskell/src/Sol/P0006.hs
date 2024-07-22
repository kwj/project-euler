module Sol.P0006 (compute, solve) where

sumOfSquares :: Int -> Int
sumOfSquares n = n * (n + 1) * (2 * n + 1) `div` 6

squareOfSum :: Int -> Int
squareOfSum n = (n * (n + 1) `div` 2) ^ (2 :: Int)

compute :: Int -> String
compute limit =
    show . abs $ sumOfSquares limit - squareOfSum limit

solve :: String
solve = compute 100
