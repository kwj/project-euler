module Sol.P0019 (compute, solve) where

import Mylib.Util (initExn)

compute :: String
compute =
    show
        . length
        . filter (== 6)
        $ scanl (\acc x -> (acc + x) `mod` 7) 0 (365 : days)
  where
    days :: [Int]
    days =
        initExn -- drop 'Dec 2000'
            . concat
            . take 25 -- from 'Jan 1901' to 'Dec 2000'
            $ repeat (commonYear ++ commonYear ++ commonYear ++ leapYear)
    commonYear = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    leapYear = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

solve :: String
solve = compute
