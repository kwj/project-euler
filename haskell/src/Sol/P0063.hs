module Sol.P0063 (compute, solve) where

{-
  n - 1 <= log10(m^n) < n    [m>0, n>0]
    <--> n - 1 <= n * log10(m) < n

    --> log10(m) < 1
    --> m < 10
   and
    --> (n - 1)/n <= log10(m)
    --> n/n - (n -1)/n >= 1 - log10(m)
    --> 1/n >= 1 - log10(m)
    --> 1/(1 - log10(m)) >= n
    --> n_{max} = floor(1/(1 - log10(m)))
-}

compute :: String
compute =
    show
        . sum
        $ map
            (\m -> floor (1 / (1 - logBase 10 m) :: Double) :: Int)
            [1 .. 9]

solve :: String
solve = compute
