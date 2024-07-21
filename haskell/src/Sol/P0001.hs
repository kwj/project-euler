module Sol.P0001 (compute, solve) where

sumOfMultiples :: Int -> Int -> Int
sumOfMultiples n limit =
    div ((n + (upper - mod upper n)) * div upper n) 2
  where
    upper = pred limit

compute :: Int -> String
compute limit =
    show $ sumOfMultiples 3 limit + sumOfMultiples 5 limit - sumOfMultiples 15 limit

solve :: String
solve = compute 1000
