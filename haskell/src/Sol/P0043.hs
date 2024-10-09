module Sol.P0043 (compute, solve) where

import Mylib.Util (headExn)

compute :: String
compute =
    show
        . sum
        . map (read :: String -> Int)
        . filter ((/= '0') . headExn)
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
            , x `notElem` s
            , mod (read (take 3 (x : s)) :: Int) d == 0
            ]

solve :: String
solve = compute
