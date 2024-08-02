module Sol.P0017 (compute, solve) where

import Data.Array.Unboxed (UArray, listArray, (!))

under20 :: UArray Int Int
under20 =
    listArray (0, 19) $
        length
            <$> [ ""
                , "one"
                , "two"
                , "three"
                , "four"
                , "five"
                , "six"
                , "seven"
                , "eight"
                , "nine"
                , "ten"
                , "eleven"
                , "twelve"
                , "thirteen"
                , "fourteen"
                , "fifteen"
                , "sixteen"
                , "seventeen"
                , "eighteen"
                , "nineteen"
                ]

mults10 :: UArray Int Int
mults10 =
    listArray (0, 9) $
        length
            <$> [ ""
                , ""
                , "twenty"
                , "thirty"
                , "forty"
                , "fifty"
                , "sixty"
                , "seventy"
                , "eighty"
                , "ninety"
                ]

countLetters :: Int -> Int
countLetters n
    | n <= 0 || n > 1000 = error "range error"
    | n == 1000 = (under20 ! 1) + (length "thousand")
    | n < 20 = under20 ! n
    | n < 100 = (mults10 ! (div n 10)) + (under20 ! (mod n 10))
    | otherwise =
        let (q, r) = (div n 100, mod n 100)
         in case r of
                0 -> (under20 ! q) + (length "hundred")
                _ -> (under20 ! q) + (length "hundred") + (length "and") + (countLetters r)

compute :: Int -> String
compute limit =
    show . sum $ countLetters <$> [1 .. limit]

solve :: String
solve = compute 1_000
