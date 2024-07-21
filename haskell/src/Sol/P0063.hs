module Sol.P0063 (compute, solve) where

compute :: String
compute =
    show
        . sum
        $ map
            (\m -> floor (1 / (1 - logBase 10 m) :: Double) :: Int)
            [1 .. 9]

solve :: String
solve = compute
