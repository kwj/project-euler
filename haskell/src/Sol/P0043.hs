module Sol.P0043 (compute, solve) where

compute :: String
compute =
    show
        . sum
        . map (read :: String -> Int)
        $ aux [1, 1, 17, 13, 11, 7, 5, 3, 2, 1] [""]
  where
    aux :: [Int] -> [String] -> [String]
    aux [] lst = lst
    aux (d : ds) lst =
        aux
            ds
            [ x : s
            | x <- "0123456789"
            , s <- lst
            , notElem x s
            , mod (read (take 3 (x : s)) :: Int) d == 0
            ]

solve :: String
solve = compute
