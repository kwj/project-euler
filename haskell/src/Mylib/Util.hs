module Mylib.Util (
    zipWith',
    headExn,
    tailExn,
    lastExn,
    initExn,
    bitLength,
    partitionByStep,
    digits,
    undigits,
    wordsWhen,
) where

import Data.List (uncons, unfoldr, unsnoc)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f (x : xs) (y : ys) =
    z : zipWith' f xs ys
  where
    !z = f x y
zipWith' _ _ _ = []

headExn :: [a] -> a
headExn lst = maybe (error "head") fst (uncons lst)

tailExn :: [a] -> [a]
tailExn lst = maybe (error "tail") snd (uncons lst)

lastExn :: [a] -> a
lastExn lst = maybe (error "last") snd (unsnoc lst)

initExn :: [a] -> [a]
initExn lst = maybe (error "init") fst (unsnoc lst)

bitLength :: Integral int => int -> Int
bitLength = length . takeWhile (> 0) . iterate (`div` 2)
{-# SPECIALIZE bitLength :: Int -> Int #-}
{-# SPECIALIZE bitLength :: Integer -> Int #-}

partitionByStep :: Int -> Int -> [a] -> [[a]]
partitionByStep n step lst =
    if length xs == n then xs : partitionByStep n step (drop step lst) else []
  where
    xs = take n lst

digits :: Integral int => int -> [int]
digits = unfoldr aux
  where
    aux 0 = Nothing
    aux x = Just (mod x 10, div x 10)
{-# SPECIALIZE digits :: Int -> [Int] #-}
{-# SPECIALIZE digits :: Integer -> [Integer] #-}

undigits :: Integral int => [int] -> int
undigits = foldr (\x acc -> acc * 10 + x) 0
{-# SPECIALIZE undigits :: [Int] -> Int #-}
{-# SPECIALIZE undigits :: [Integer] -> Integer #-}

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =
    aux (break p s) []
  where
    aux :: (String, String) -> [String] -> [String]
    aux ("", "") acc = reverse acc
    aux ("", tl) acc = aux (break p (drop 1 tl)) acc
    aux (hd, tl) acc = aux (break p (drop 1 tl)) (hd : acc)
