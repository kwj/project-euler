module Sol.P0045 (compute, solve) where

gpeSols :: [(Int, Int)]
gpeSols = iterate (\(x, y) -> (2 * x + 3 * y, x + 2 * y)) (1, 1)

compute :: Int -> String
compute nth =
    show $ j * ((2 * j) - 1)
  where
    (_, z) = (filter (\(x, y) -> mod x 6 == 5 && mod y 4 == 3) gpeSols) !! (nth - 1)
    j = (z + 1) `div` 4

solve :: String
solve = compute 3
