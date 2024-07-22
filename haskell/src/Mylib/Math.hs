module Mylib.Math (
    extGcd,
    invMod,
    powerMod,
    powerModExn,
    isqrt,
    kronecker,
    binomial,
    factorial,
    maxPower,
    numOfDigits,
    getCTZ,
    isPalindrome,
    isPandigital,
    isPandigitalNZ,
    isTriangular,
    isSquare,
    isPentagonal,
    isHexagonal,
) where

import Data.Bits (shiftL, (.|.))
import Data.Maybe (fromMaybe)

import Mylib.Util (bitLength)

extGcd :: Integral int => int -> int -> (int, int, int)
extGcd 0 0 = (0, 0, 0)
extGcd a b =
    let (g, x, y) = extGcd' (abs a) (abs b)
     in (g, sgn a * x, sgn b * y)
  where
    sgn x = if x < 0 then -1 else 1
    extGcd' x 0 = (abs x, 1, 0)
    extGcd' x y =
        let (g, s, t) = extGcd' y (mod x y)
         in (g, t, s - div x y * t)
{-# SPECIALIZE extGcd :: Int -> Int -> (Int, Int, Int) #-}
{-# SPECIALIZE extGcd :: Integer -> Integer -> (Integer, Integer, Integer) #-}

invMod :: Integral int => int -> int -> Maybe int
invMod _ 0 = Nothing -- modulo must not be zero
invMod a m
    | g /= 1 = Nothing -- there is no modular multiplicative inverse
    | otherwise = Just result
  where
    (g, result, _) = extGcd a m
{-# SPECIALIZE invMod :: Int -> Int -> Maybe Int #-}
{-# SPECIALIZE invMod :: Integer -> Integer -> Maybe Integer #-}

powerMod :: Int -> Int -> Int -> Maybe Int
powerMod _ _ 0 = Nothing -- "modulo must not be zero"
powerMod b e m
    | e < 0 = do
        case invMod b m of
            Just x ->
                Just
                    (fromIntegral (powerMod' (fromIntegral x) (fromIntegral (-e)) (fromIntegral m) 1))
            Nothing -> Nothing
    | otherwise =
        Just
            (fromIntegral (powerMod' (fromIntegral b) (fromIntegral e) (fromIntegral m) 1))

powerMod' :: Integer -> Integer -> Integer -> Integer -> Integer
powerMod' b e m result
    | e == 0 = result
    | odd e = powerMod' ((b * b) `mod` m) (e `div` 2) m ((result * b) `mod` m)
    | otherwise = powerMod' ((b * b) `mod` m) (e `div` 2) m result

powerModExn :: Int -> Int -> Int -> Int
powerModExn b e m = fromMaybe (error "") (powerMod b e m)

isqrt :: Integral int => int -> int
isqrt n
    | n == 0 = 0
    | n < 0 = error "argument must not be negative"
    | otherwise =
        let a = isqrt' (div (bitLength n - 1) 2) n
         in if n < a * a then a - 1 else a
  where
    isqrt' 0 _ = 1
    isqrt' c n' =
        let k = div (c - 1) 2
            a = isqrt' (div c 2) (div n' (2 ^ (2 * k + 2)))
         in a * 2 ^ k + div (div n' (2 ^ (k + 2))) a
{-# SPECIALIZE isqrt :: Int -> Int #-}
{-# SPECIALIZE isqrt :: Integer -> Integer #-}

kronecker :: Integral int => int -> int -> int
kronecker a 0 = if abs a == 1 then 1 else 0
kronecker a n
    | n < 0 =
        if a < 0 then kronecker' (-1) a (negate n) else kronecker' 1 a (negate n)
    | otherwise = kronecker' 1 a n
{-# SPECIALIZE kronecker :: Int -> Int -> Int #-}
{-# SPECIALIZE kronecker :: Integer -> Integer -> Integer #-}

kronecker' :: Integral int => int -> int -> int -> int
kronecker' sign a n
    | even n =
        if even a
            then
                0
            else
                let (new_n, new_sign) = auxKronecker sign n a
                 in kronecker'' new_sign (mod a new_n) new_n
    | otherwise = kronecker'' sign (mod a n) n
{-# SPECIALIZE kronecker' :: Int -> Int -> Int -> Int #-}
{-# SPECIALIZE kronecker' :: Integer -> Integer -> Integer -> Integer #-}

kronecker'' :: Integral int => int -> int -> int -> int
kronecker'' sign 0 1 = sign
kronecker'' _ 0 _ = 0
kronecker'' sign a n =
    kronecker'' new_sign (mod n new_a) new_a
  where
    (new_a, tmp_sign) = auxKronecker sign a n
    new_sign = if mod new_a 4 == 3 && mod n 4 == 3 then negate tmp_sign else tmp_sign
{-# SPECIALIZE kronecker'' :: Int -> Int -> Int -> Int #-}
{-# SPECIALIZE kronecker'' :: Integer -> Integer -> Integer -> Integer #-}

auxKronecker :: Integral int => int -> int -> int -> (int, int)
auxKronecker sign x y
    | odd x = (x, sign)
    | otherwise = (x `div` (2 ^ ntz_x), new_sign)
  where
    ntz_x = getCTZ x
    new_sign =
        if even ntz_x
            then
                sign
            else
                let tmp = y `mod` 8
                 in if tmp == 3 || tmp == 5 then (-sign) else sign
{- FOURMOLU_DISABLE -}
{-# SPECIALIZE auxKronecker :: Int -> Int -> Int -> (Int, Int) #-}
{-# SPECIALIZE auxKronecker :: Integer -> Integer -> Integer -> (Integer, Integer) #-}
{- FOURMOLU_ENABLE -}

binomial :: Integral int => int -> int -> int
binomial _ 0 = 1
binomial n k
    | n == k = 1
    | n < k = 0
    | (div n 2) < k = binomial n (n - k)
    | otherwise = foldl (\acc i -> acc * (n - k + i) `div` i) 1 [1 .. k]
{-# SPECIALIZE binomial :: Int -> Int -> Int #-}
{-# SPECIALIZE binomial :: Integer -> Integer -> Integer #-}

factorial :: Integral int => int -> int
factorial 0 = 1
factorial n = n * factorial (n - 1)
{-# SPECIALIZE factorial :: Int -> Int #-}
{-# SPECIALIZE factorial :: Integer -> Integer #-}

maxPower :: Integral int => int -> int -> Int
maxPower n base =
    aux n 0
  where
    aux x e
        | x >= base = aux (div x base) (succ e)
        | otherwise = e
{-# SPECIALIZE maxPower :: Int -> Int -> Int #-}
{-# SPECIALIZE maxPower :: Integer -> Integer -> Int #-}

numOfDigits :: Integral int => int -> int -> Int
numOfDigits n base = (maxPower n base) + 1
{-# SPECIALIZE numOfDigits :: Int -> Int -> Int #-}
{-# SPECIALIZE numOfDigits :: Integer -> Integer -> Int #-}

getCTZ :: Integral int => int -> Int
getCTZ n = getCTZ' n 0
  where
    getCTZ' x cnt = if odd x then cnt else getCTZ' (x `div` 2) (succ cnt)
{-# SPECIALIZE getCTZ :: Int -> Int #-}
{-# SPECIALIZE getCTZ :: Integer -> Int #-}

isPalindrome :: Integral int => int -> int -> Bool
isPalindrome n base = (isPalindrome' n 0) == n
  where
    isPalindrome' x acc =
        if x == 0
            then
                acc
            else
                isPalindrome' (x `div` base) (acc * base + (x `mod` base))
{-# SPECIALIZE isPalindrome :: Int -> Int -> Bool #-}
{-# SPECIALIZE isPalindrome :: Integer -> Integer -> Bool #-}

isPandigital :: Int -> Bool
isPandigital n =
    (makeBits n 0) == (shiftL 1 ((maxPower n 10) + 1)) - 1
  where
    makeBits :: Int -> Int -> Int
    makeBits 0 bits = bits
    makeBits i bits = makeBits (div i 10) ((.|.) bits (shiftL 1 (mod i 10)))

isPandigitalNZ :: Int -> Bool
isPandigitalNZ n =
    checkZero n && isPandigital (n * 10)
  where
    checkZero :: Int -> Bool
    checkZero 0 = True
    checkZero i
        | (mod i 10) == 0 = False
        | otherwise = checkZero (div i 10)

isTriangular :: Integral int => int -> Bool
isTriangular n =
    mod tmp_sqrt 2 == 1 && tmp_sqrt * tmp_sqrt == tmp
  where
    tmp = 8 * n + 1
    tmp_sqrt = isqrt tmp
{-# SPECIALIZE isTriangular :: Int -> Bool #-}
{-# SPECIALIZE isTriangular :: Integer -> Bool #-}

isSquare :: Integral int => int -> Bool
isSquare n =
    n_sqrt * n_sqrt == n
  where
    n_sqrt = isqrt n
{-# SPECIALIZE isSquare :: Int -> Bool #-}
{-# SPECIALIZE isSquare :: Integer -> Bool #-}

isPentagonal :: Integral int => int -> Bool
isPentagonal n =
    mod tmp_sqrt 6 == 5 && tmp_sqrt * tmp_sqrt == tmp
  where
    tmp = 24 * n + 1
    tmp_sqrt = isqrt tmp
{-# SPECIALIZE isPentagonal :: Int -> Bool #-}
{-# SPECIALIZE isPentagonal :: Integer -> Bool #-}

isHexagonal :: Integral int => int -> Bool
isHexagonal n =
    mod tmp_sqrt 4 == 3 && tmp_sqrt * tmp_sqrt == tmp
  where
    tmp = 8 * n + 1
    tmp_sqrt = isqrt tmp
{-# SPECIALIZE isHexagonal :: Int -> Bool #-}
{-# SPECIALIZE isHexagonal :: Integer -> Bool #-}
