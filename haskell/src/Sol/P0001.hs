module Sol.P0001 (compute, solve) where

compute :: Int -> String
compute limit =
    show $ sumOfMultiples 3 + sumOfMultiples 5 - sumOfMultiples 15
  where
    upper = pred limit

    sumOfMultiples :: Int -> Int
    sumOfMultiples n =
        div ((n + (upper - mod upper n)) * div upper n) 2

solve :: String
solve = compute 1000
