module Sol.P0062 (compute, solve) where

import Data.List (sort)

import qualified Data.Map.Strict as M (Map, empty, insert, lookup)

import Mylib.Util (lastExn)

compute :: Int -> String
compute len =
    show $ aux M.empty [1 ..]
  where
    aux :: M.Map String [Int] -> [Int] -> Int
    aux _ [] = error "unreachable"
    aux tbl (n : ns)
        | Just v <- M.lookup key tbl =
            if length v == len - 1
                then lastExn v
                else aux (M.insert key (cube : v) tbl) ns
        | otherwise =
            aux (M.insert key [cube] tbl) ns
      where
        cube = n ^ (3 :: Int)
        key = sort $ show cube

solve :: String
solve = compute 5
