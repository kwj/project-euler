module Sol.P0005 (compute, solve) where

compute :: Int -> String
compute limit
    | limit <= 0 = error "The argument must be positive"
    | otherwise = show $ foldl1 lcm [1 .. limit]

solve :: String
solve = compute 20
