module Sol.P0068 (compute, solve) where

import Data.List (delete, sortBy)

import Mylib.Util (headExn, initExn, lastExn)

searchRings :: Int -> [[Int]]
searchRings n_gon =
    concatMap searchRings' [min_weight .. max_weight]
  where
    numbers = [1 .. n_gon * 2]
    -- weight: sum of each node on line
    --   minimum weight: (n_gon * 2) + 1 + 2 = n_gon * 2 + 3
    --   maximum weight: 1 + (n_gon * 2 - 1) + (n_gon * 2) = n_gon * 4
    min_weight = n_gon * 2 + 3
    max_weight = n_gon * 4

    searchRings' :: Int -> [[Int]]
    searchRings' weight =
        concatMap (\n -> aux [n] (delete n numbers)) numbers
      where
        -- ring :: [Int]
        --    +-+---     ---+-+-+   X: first selected inner node (last ring)
        --    |Z|    ...    |Y|X|   Y: first selected outer node (last $ init ring)
        --    +-+---     ---+-+-+   Z: last selelected inner node (head ring)
        --     0
        --
        --     [Y]
        --       \
        --        [X]   *
        --       /   \ /
        --     ??     *
        --    / \    /
        --  ??  [Z]-*-- *
        --        \
        --         ??
        aux :: [Int] -> [Int] -> [[Int]]
        aux ring [last_number] =
            [ [last_number, headExn ring, lastExn ring]
            | weight == last_number + headExn ring + lastExn ring
            , last_number > (lastExn $ initExn ring)
            ]
        aux ring rest =
            [ outer : headExn ring : inner : result
            | outer <- rest
            , (length ring > 1 && outer > (lastExn $ initExn ring))
                || (length ring == 1 && outer <= n_gon + 1)
            , let inner = weight - headExn ring - outer
            , outer /= inner
            , elem inner rest
            , result <- aux (inner : outer : ring) (delete inner (delete outer rest))
            ]

listToDigitString :: [Int] -> String
listToDigitString =
    foldl1 (++) . map show

compute :: Int -> String
compute n_gon =
    headExn
        . sortBy (flip compare)
        $ if n_gon == 5
            then
                -- only 16-digit strings when n_gon == 5
                filter ((== 16) . length) . map listToDigitString $ searchRings n_gon
            else
                map listToDigitString $ searchRings n_gon

solve :: String
solve = compute 5
