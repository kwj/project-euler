module Sol.P0061 (compute, solve) where

import Data.List (delete, nub)

import Mylib.Util (headExn, lastExn)

-- P(s,n)
-- A polygonal number is equal to an arithmetic series.
polygonalNumbers :: Int -> [Int]
polygonalNumbers s =
    filter (\x -> mod x 100 >= 10)
        . takeWhile (< 10000)
        . dropWhile (< 1000)
        $ scanl1 (+) [1, k ..]
  where
    k = s - 1

-- Triangle/Square/.../Heptagonal numbers
polygonals_P3_to_P7 :: [[Int]]
polygonals_P3_to_P7 =
    polygonalNumbers <$> [3 .. 7]

searchRest :: [Int] -> [[Int]] -> [[Int]]
searchRest ns [] =
    [[] | headExn ns `mod` 100 == lastExn ns `div` 100] -- cyclic?
searchRest ns pss =
    [ p : rest
    | ps <- pss -- select a group of polygonals
    , p <- ps -- select a polygonal number from the group
    , mod (headExn ns) 100 == p `div` 100 -- can it be connected?
    , rest <- searchRest (p : ns) (delete ps pss) -- If ok, search for next connectable polygonal number.
    ]

compute :: String
compute =
    -- According to the problem statement, only one cycle exists.
    if length cands /= 1
        then error "no answer exists"
        else show . sum $ headExn cands
  where
    cands =
        filter (\lst -> length lst == 6) $ -- each number in the cycle is different
            map
                nub
                [ x : rest
                | x <- polygonalNumbers 8 -- start searching from octagonal numbers
                , rest <- searchRest [x] polygonals_P3_to_P7
                ]

solve :: String
solve = compute
