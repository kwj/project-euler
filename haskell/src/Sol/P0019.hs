module Sol.P0019 (compute, solve) where

import Mylib.Util (initExn)

compute :: String
compute =
    show
        . length
        . filter (== 6) -- '6' indicates Sunday because Jan 1, 1900 was a Monday.
        $ scanl (\acc x -> (acc + x) `mod` 7) 0 (365 : days) -- 365 is the number of days in 1900
  where
    -- the number of days in each month (Jan 1901 - Nov 2000)
    days :: [Int]
    days =
        initExn -- drop 'Dec 2000'
            . concat -- from 'Jan 1901' to 'Dec 2000'
            $ replicate 25 (commonYear ++ commonYear ++ commonYear ++ leapYear)
    commonYear = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    leapYear = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

solve :: String
solve = compute
