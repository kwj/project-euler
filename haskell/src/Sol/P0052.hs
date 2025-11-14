module Sol.P0052 (compute, solve) where

import Data.List (find, sort)
import Data.Maybe (fromJust)

import Mylib.Util (digits)

isPermutedMultiples :: Int -> Bool
isPermutedMultiples n =
    all ((== (sort $ digits n)) . sort . digits . (n *)) [2 .. 6]

compute :: String
compute =
    show
        . fromJust
        . find isPermutedMultiples
        $ (\x -> [10 ^ (x - 1) .. (10 ^ x `div` 6)]) =<< [6 :: Int ..]

solve :: String
solve = compute
