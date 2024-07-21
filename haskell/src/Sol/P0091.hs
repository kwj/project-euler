module Sol.P0091 (compute, solve) where

compute :: Int -> Int -> String
compute x_size y_size =
    show $
        (x_size * y_size) * 3
            + 2
                * sum
                    [ min (y * gcd x y `div` x) ((x_size - x) * gcd x y `div` y)
                    | x <- [1 .. x_size]
                    , y <- [1 .. y_size]
                    ]

solve :: String
solve = compute 50 50
