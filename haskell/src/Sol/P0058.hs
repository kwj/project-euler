module Sol.P0058 (compute, solve) where

import Mylib.Prime (isPrime)

compute :: String
compute =
    show $ aux 1 0 1
  where
    aux :: Int -> Int -> Int -> Int
    aux base nPrimes n
        | next_nPrimes * 10 < 4 * n + 1 =
            2 * n + 1
        | otherwise =
            aux (base + 8 * n) next_nPrimes (succ n)
      where
        wrapper x = if isPrime x then 1 else 0
        next_nPrimes =
            nPrimes
                + wrapper (base + 2 * n)
                + wrapper (base + 4 * n)
                + wrapper (base + 6 * n)

solve :: String
solve = compute
