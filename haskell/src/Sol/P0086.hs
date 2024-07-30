module Sol.P0086 (compute, solve) where

{-
This implementation is little slow. The following is a result on my Raspberry Pi 4.

% cabal v2-run pe-solver -- 86
[Problem 86]
Answer: 1818
Elapsed time: 1.393238 sec.
-}

{-
  1 <= a, b, c <= M

  we can ignore rotations. there is only one case to consider.
    1 <= a <= b <= c <= M
     --> 2 <= a + b <= 2c

      +--------F
      |        |      * sqrt(c^2 + (a+b)^2) must be an integer
      |--------|
      |        | a+b >= 2
      |        |
      S--------+
           c

  when a+b <= c <= M
    write a+b = x
      (a, b) = (1, x-1), (2, x-2), ..., (x-1, 1)
    however, because a<=b
      num of (a,b) = floor(x/2) = floor((a+b)/2)

  when a+b > c
      num of (a,b) = floor((a+b)/2) - ((a+b-1) - c)

      example: c=10, a+b=15
        (a,b) = (1,14), ..., (5,10), (6,9), (7,8), ..., (14,1)
                             ####################
                ^^^^^^^^^^^ = (a+b-1) - c
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ = floor((a+b)/2)

      example: c=10, a+b=16
        (a,b) = (1,15), ..., (6,10), (7,9), (8,8), ..., (15,1)
                             ####################
                ^^^^^^^^^^^ = (a+b-1) - c
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ = floor((a+b)/2)
-}

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
        $ scanl1 (+) (countTriangles <$> [init_c ..])
  where
    init_c = 3 :: Int

solve :: String
solve = compute 1_000_000
