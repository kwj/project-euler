module Sol.P0090 (compute, solve) where

import Mylib.Combinatorics (combinations, combinationsWithRepetition)

squares :: [(Int, Int)]
squares =
    [ (0, 1) -- 1^2
    , (0, 4) -- 2^2
    , (0, 6) -- 3^2
    , (1, 6) -- 4^2
    , (2, 5) -- 5^2
    , (3, 6) -- 6^2
    , (4, 6) -- 7^2, 8^2
    , (8, 1) -- 9^2
    ]

{- HLINT ignore isContained "Use head" -}
isContained :: [[Int]] -> (Int, Int) -> Bool
isContained twoDice sq =
    elem (fst sq) (twoDice !! 0) && elem (snd sq) (twoDice !! 1)
        || elem (fst sq) (twoDice !! 1) && elem (snd sq) (twoDice !! 0)

compute :: String
compute =
    show
        . length
        . filter (\twoDice -> all (isContained twoDice) squares)
        . combinationsWithRepetition 2
        $ combinations 6 [0, 1, 2, 3, 4, 5, 6, 7, 8, 6]

solve :: String
solve = compute
