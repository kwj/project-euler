module Sol.P0017 (compute, solve) where

import Data.Array.Unboxed (UArray, listArray, (!))

{- FOURMOLU_DISABLE -}
under20 :: UArray Int Int
under20 =
    listArray
        (0, 19)
        [ 0, length "one", length "two", length "three", length "four"
        , length "five", length "six", length "seven", length "eight", length "nine"
        , length "ten", length "eleven", length "twelve", length "thirteen", length "fourteen"
        , length "fifteen", length "sixteen", length "seventeen", length "eighteen", length "nineteen"
        ]

mults10 :: UArray Int Int
mults10 =
    listArray
        (0, 9)
        [ 0, 0, length "twenty", length "thirty", length "forty"
        , length "fifty", length "sixty", length "seventy", length "eighty", length "ninety"
        ]
{- FOURMOLU_ENABLE -}

countLetters :: Int -> Int
countLetters n
    | n <= 0 || n > 1000 = error "range error"
    | n == 1000 = (under20 ! 1) + (length "thousand")
    | n < 20 = under20 ! n
    | n < 100 = (mults10 ! (div n 10)) + (under20 ! (mod n 10))
    | otherwise =
        let (q, r) = (div n 100, mod n 100)
         in if r == 0
                then
                    (under20 ! q) + (length "hundred")
                else
                    (under20 ! q) + (length "hundred") + (length "and") + (countLetters r)

compute :: Int -> String
compute limit = show $ sum $ map countLetters [1 .. limit]

solve :: String
solve = compute 1_000
