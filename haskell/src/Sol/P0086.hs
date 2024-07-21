module Sol.P0086 (compute, solve) where

import Mylib.Math (isSquare)

countTriangles :: Int -> Int
countTriangles c =
    aux (2 * c) 0
  where
    aux :: Int -> Int -> Int
    aux 2 acc = acc
    aux ab acc
        | isSquare (c * c + ab * ab) == False =
            aux (pred ab) acc
        | ab <= c =
            aux (pred ab) (acc + ab `div` 2)
        | otherwise =
            aux (pred ab) (acc + ab `div` 2 - (ab - 1 - c))

compute :: Int -> String
compute boundary =
    show
        . (+ init_c)
        . length
        . takeWhile (<= boundary)
        $ scanl1 (+) (map countTriangles [init_c ..])
  where
    init_c = 3 :: Int

solve :: String
solve = compute 1_000_000
