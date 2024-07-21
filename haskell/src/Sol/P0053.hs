module Sol.P0053 (compute, solve) where

compute :: Int -> Int -> String
compute upper_n thr =
    show $
        if thr /= 0
            then aux upper_n upper_n upper_n 1 0
            else aux upper_n upper_n upper_n 1 (upper_n * 2)
  where
    aux ::
        Int -> -- n
        Int -> -- x
        Int -> -- c
        Int -> -- r
        Int -> -- answer
        Int
    aux n x c r answer
        | r <= (div n 2) =
            if c > thr
                then
                    aux (pred n) (pred x) (div (pred x * c) n) r (answer + n - r * 2 + 1)
                else
                    aux n (pred x) (div (pred x * c) (succ r)) (succ r) answer
        | otherwise =
            answer

solve :: String
solve = compute 100 1_000_000
