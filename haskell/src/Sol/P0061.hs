module Sol.P0061 (compute, solve) where

import Data.List (delete, nub)

import Mylib.Util (headExn, lastExn)

searchRest :: [Int] -> [[Int]] -> [[Int]]
searchRest ns [] =
    [[] | headExn ns `mod` 100 == lastExn ns `div` 100] -- cyclic?
searchRest ns pss =
    [ p : rest
    | ps <- pss -- select a group of polygonals
    , p <- ps -- select a polygonal number from the group
    , headExn ns `mod` 100 == p `div` 100 -- can it be connected?
    , rest <- searchRest (p : ns) (delete ps pss) -- If ok, search for next connectable polygonal number.
    ]

compute :: Int -> String
compute maxNumSidesPolygon =
    -- According to the problem statement, only one cycle exists.
    if length cands /= 1
        then error "fatal error (only one answer must exist)"
        else show . sum $ headExn cands
  where
    cands =
        filter ((== maxNumSidesPolygon - 2) . length) $ -- all numbers in a cycle are different from each other
            [ nub (x : rest)
            | x <- polygonalNumbers maxNumSidesPolygon
            , rest <- searchRest [x] (polygonalNumbers <$> [3 .. maxNumSidesPolygon - 1])
            ]

    polygonalNumbers :: Int -> [Int]
    polygonalNumbers s =
        filter ((>= 10) . (`mod` 100))
            . takeWhile (< 10000)
            . dropWhile (< 1000)
            $ scanl1 (+) [1, (s - 1) ..]

solve :: String
solve = compute 8
