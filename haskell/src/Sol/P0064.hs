module Sol.P0064 (compute, solve) where

{-
            sqrt(N) + b0        1              1
  sqrt(N) = ------------ = a0 + --,  x1 = a1 + --, ...
                 c0             x1             x2

                  c0             c0(sqrt(N) - (b0 - a0c0))
    x1 = --------------------- = -------------------------
         sqrt(N) + (b0 - a0c0)       N - (b0 - a0c0)^2

         sqrt(N) + (a0c0 - b0)   sqrt(N) + b1         1
       = --------------------- = ------------- = a1 + --
           N - (a0c0 - b0)^2          c1              x2
           -----------------
                  c0
   -->
     a{n} = floor( (sqrt(N)+b{n}) / c{n} )
     b{n+1} = a{n}*c{n} - b{n}
     c{n+1} = (N - b{n+1}^2) / c{n}

     b{0} = 0, c{0} = 1, a{0} = sqrt(N)
-}

import Mylib.Math (isqrt)

continuedFraction :: Int -> (Int, [Int])
continuedFraction n
    | isqrt_n * isqrt_n == n =
        (isqrt_n, [])
    | otherwise =
        aux 0 1 isqrt_n []
  where
    isqrt_n = isqrt n
    stop = 2 * isqrt_n

    aux :: Int -> Int -> Int -> [Int] -> (Int, [Int])
    aux b c a lst
        | a == stop =
            (isqrt_n, reverse lst)
        | otherwise =
            let b' = a * c - b
                c' = (n - b' * b') `div` c
                a' = (isqrt_n + b') `div` c'
             in aux b' c' a' (a' : lst)

compute :: Int -> String
compute limit =
    show
        . length
        . filter (odd . length . snd)
        $ map continuedFraction [1 .. limit]

solve :: String
solve = compute 10_000
