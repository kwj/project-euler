module Sol.P0049 (compute, solve) where

import Control.Arrow ((&&&))
import Data.List (sort)

import qualified Data.Map.Strict as M (
    Map,
    elems,
    empty,
    filter,
    insert,
    member,
    update,
 )

import Mylib.Combinatorics (combinations)
import Mylib.Prime (primes)
import Mylib.Util (digits, headExn, undigits)

makePrimeTbl :: Int -> M.Map String [Int]
makePrimeTbl ndigits =
    addEntry M.empty ((show . undigits . sort . digits &&& id) <$> ps)
  where
    ps = primes (10 ^ (ndigits - 1)) (10 ^ ndigits)

    addEntry :: M.Map String [Int] -> [(String, Int)] -> M.Map String [Int]
    addEntry m [] = m
    addEntry m ((key, val) : rest)
        | M.member key m = addEntry (M.update (Just . (val :)) key m) rest
        | otherwise = addEntry (M.insert key [val] m) rest

findNumbers :: [[Int]] -> [Int]
findNumbers llst
    | length cands == 1 = reverse $ headExn cands
    | otherwise = error "unreachable"
  where
    cands =
        filter (\lst -> (lst !! 0) /= 8147 && (lst !! 2) /= 1487) $
            filter (\lst -> (lst !! 0) - (lst !! 1) == (lst !! 1) - (lst !! 2)) llst

compute :: String
compute =
    foldl1 (++)
        . map show
        . findNumbers
        . concatMap (combinations lowerLimit)
        . M.elems
        . M.filter ((>= lowerLimit) . length)
        $ makePrimeTbl nDigits
  where
    nDigits = 4
    lowerLimit = 3

solve :: String
solve = compute
