module Sol.P0014 (compute, solve) where

-- It is a straightforward method that doesn't use memoization.
getCollatzLength :: Int -> Int
getCollatzLength n = aux n 1
  where
    aux 1 cnt = cnt
    aux x cnt = aux next_x (succ cnt)
      where
        next_x = if even x then div x 2 else 3 * x + 1

compute :: Int -> String
compute limit =
    show . snd . maximum $
        map (\x -> (getCollatzLength x, x)) [(div limit 2) .. (limit - 1)]

solve :: String
solve = compute 1_000_000
