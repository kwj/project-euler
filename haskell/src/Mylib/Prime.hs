module Mylib.Prime (
    primeNumbers,
    primes,
    isPrime,
    nextPrime,
    prevPrime,
) where

import Control.Arrow ((&&&))
import Data.Array.Unboxed (UArray, listArray, (!))
import Data.Bits (Bits (..), FiniteBits (..))
import Data.List (find, unfoldr)
import Data.Maybe (fromMaybe)
import Data.Word (Word32, Word64)

import qualified Data.Numbers.Primes as P (primes)

import Mylib.Math (getCTZ, isqrt, kronecker, powerModExn)
import Mylib.MinFactor (minFactorTbl)
import Mylib.Util (bitLength, headExn, lastExn)

primeNumbers :: [Int]
primeNumbers = P.primes

primes :: Int -> Int -> [Int]
primes low high =
    if low <= high
        then
            takeWhile (<= high) $ dropWhile (< low) primeNumbers
        else
            error "range error"

-- One-shot deterministic Miller–Rabin primality test for UInt32 (FJ32_256)
-- http://ceur-ws.org/Vol-1326/020-Forisek.pdf

{- FOURMOLU_DISABLE -}
sprpBase :: Word32 -> Word32
sprpBase n = bases ! idx
  where
    n' = fromIntegral n :: Word64
    h1 = (shiftR n' 16 `xor` n') * 0x45d9f3b
    h2 = (shiftR h1 16 `xor` h1) * 0x45d9f3b
    idx = (shiftR h2 16 `xor` h2) .&. 0xff
    bases :: UArray Word64 Word32
    bases =
        listArray
            (0, 255)
            [ 15591, 2018, 166, 7429, 8064, 16045, 10503, 4399, 1949, 1295, 2776, 3620, 560, 3128, 5212, 2657
            , 2300, 2021, 4652, 1471, 9336, 4018, 2398, 20462, 10277, 8028, 2213, 6219, 620, 3763, 4852, 5012
            , 3185, 1333, 6227, 5298, 1074, 2391, 5113, 7061, 803, 1269, 3875, 422, 751, 580, 4729, 10239
            , 746, 2951, 556, 2206, 3778, 481, 1522, 3476, 481, 2487, 3266, 5633, 488, 3373, 6441, 3344
            , 17, 15105, 1490, 4154, 2036, 1882, 1813, 467, 3307, 14042, 6371, 658, 1005, 903, 737, 1887
            , 7447, 1888, 2848, 1784, 7559, 3400, 951, 13969, 4304, 177, 41, 19875, 3110, 13221, 8726, 571
            , 7043, 6943, 1199, 352, 6435, 165, 1169, 3315, 978, 233, 3003, 2562, 2994, 10587, 10030, 2377
            , 1902, 5354, 4447, 1555, 263, 27027, 2283, 305, 669, 1912, 601, 6186, 429, 1930, 14873, 1784
            , 1661, 524, 3577, 236, 2360, 6146, 2850, 55637, 1753, 4178, 8466, 222, 2579, 2743, 2031, 2226
            , 2276, 374, 2132, 813, 23788, 1610, 4422, 5159, 1725, 3597, 3366, 14336, 579, 165, 1375, 10018
            , 12616, 9816, 1371, 536, 1867, 10864, 857, 2206, 5788, 434, 8085, 17618, 727, 3639, 1595, 4944
            , 2129, 2029, 8195, 8344, 6232, 9183, 8126, 1870, 3296, 7455, 8947, 25017, 541, 19115, 368, 566
            , 5674, 411, 522, 1027, 8215, 2050, 6544, 10049, 614, 774, 2333, 3007, 35201, 4706, 1152, 1785
            , 1028, 1540, 3743, 493, 4474, 2521, 26845, 8354, 864, 18915, 5465, 2447, 42, 4511, 1660, 166
            , 1249, 6259, 2553, 304, 272, 7286, 73, 6554, 899, 2816, 5197, 13330, 7054, 2818, 3199, 811
            , 922, 350, 7514, 4452, 3449, 2663, 4708, 418, 1621, 1171, 3471, 88, 11345, 412, 1559, 194]
{- FOURMOLU_ENABLE -}

-- Check whether `n` is a strong probable prime, SPRP(base).
sprpTestUInt32 :: Word32 -> Word32 -> Bool
sprpTestUInt32 n base =
    cond1 || cond2
  where
    powerModW64 :: Word64 -> Word64 -> Word64 -> Word64
    powerModW64 =
        go 1
      where
        go :: Word64 -> Word64 -> Word64 -> Word64 -> Word64
        go result b e m
            | e == 0 = result
            | testBit e 0 =
                go ((result * b) `mod` m) ((b * b) `mod` m) (shiftR e 1) m
            | otherwise =
                go result ((b * b) `mod` m) (shiftR e 1) m

    n' = fromIntegral n
    base' = fromIntegral base
    s = countTrailingZeros (n' - 1)
    d = shiftR (n' - 1) s
    cond1 = powerModW64 base' d n' == 1
    cond2 = elem (n' - 1) $ map (\k -> powerModW64 base' (d * 2 ^ k) n') [0 .. (s - 1)]

-- Check whether `n` is a strong probable prime, SPRP(base).
sprpTest :: Integer -> Integer -> Bool
sprpTest n base =
    cond1 || cond2
  where
    s = getCTZ (n - 1)
    d = shiftR (n - 1) s
    cond1 = powerModExn base d n == 1
    cond2 = elem (n - 1) $ map (\k -> powerModExn base (d * 2 ^ k) n) [0 .. (s - 1)]

-- Return Just (D, P, Q) or Nothing when `n` is composite.
lucasSeqParameter :: Integer -> Maybe (Integer, Integer, Integer)
lucasSeqParameter n
    | isqrt_n * isqrt_n == n = Nothing
    | d == 5 = Just (d, 5, 5)
    | otherwise = Just (d, 1, (1 - d) `div` 4)
  where
    isqrt_n = isqrt n
    d =
        snd
            . headExn
            . dropWhile ((/= -1) . fst)
            . map (flip kronecker n &&& id)
            $ zipWith (*) (iterate (+ 2) 5) (cycle [1, -1])

-- Return (Ud, Vd, Q^d, s).
-- Note: n + 1 = d * 2^s
lucasSeq ::
    Integer -> Integer -> Integer -> Integer -> (Integer, Integer, Integer, Int)
lucasSeq n paramD paramP paramQ =
    go 1 paramP paramQ (bitLength d - 2)
  where
    s = getCTZ (n + 1)
    d = shiftR (n + 1) s

    go :: Integer -> Integer -> Integer -> Int -> (Integer, Integer, Integer, Int)
    go uk vk qk idx
        | idx < 0 = (uk, vk, qk, s)
        | even $ shiftR d idx = go nextUk nextVk nextQk (idx - 1)
        | otherwise =
            let tmpUk = (paramP * nextUk) + nextVk
                tmpVk = (paramD * nextUk) + (paramP * nextVk)
             in go
                    (shiftR (if odd tmpUk then tmpUk + n else tmpUk) 1 `mod` n)
                    (shiftR (if odd tmpVk then tmpVk + n else tmpVk) 1 `mod` n)
                    ((paramQ * nextQk) `mod` n)
                    (idx - 1)
      where
        nextUk = (uk * vk) `mod` n
        nextVk = (vk * vk - 2 * qk) `mod` n
        nextQk = (qk * qk) `mod` n

-- Check whether `n` is a strong Lucas Probable prime, slprp(P, Q).
-- If yes, return Just (Vₙ₊₁, Q⁽ⁿ⁺¹⁾ᐟ²). Otherwise, return Nothing because `n` is composite.
testSlprp ::
    Integer -> Integer -> Integer -> Integer -> Maybe (Integer, Integer)
testSlprp n paramD paramP paramQ =
    if ud == 0 || (elem 0 . map fst $ vqPairs)
        then
            let (v, q) = lastExn vqPairs in Just (nextV v q, q)
        else
            Nothing
  where
    nextV :: Integer -> Integer -> Integer
    nextV v q = (v * v - 2 * q) `mod` n

    nextQ :: Integer -> Integer
    nextQ q = (q * q) `mod` n

    (ud, vd, qd, s) = lucasSeq n paramD paramP paramQ
    -- vqPairs: [(V_d, Q^d), (V_2d, Q^2d) ... (V_(2^(s-1))*d Q^(2^(s-1))*d)]
    vqPairs = take s $ unfoldr (\(x, y) -> Just ((x, y), (nextV x y, nextQ y))) (vd, qd)

-- Check whether `n` is a Lucas-V probable prime, vprp(Q).
-- If yes, return true. Otherwise, return false because `n` is composite.
-- parameter: Vₙ₊₁, paramQ, n
testVprp :: Integer -> Integer -> Integer -> Bool
testVprp v q n = v `mod` n == (2 * q) `mod` n

-- Return false if `n` is composite.
-- Note that skip this test when (abs q) is a power of 2.
-- parameter: paramQ, Q⁽ⁿ⁺¹⁾ᐟ², n
testEulerCriterion :: Integer -> Integer -> Integer -> Bool
testEulerCriterion q q' n =
    popCount (abs q) == 1 || q' `mod` n == (q * kronecker q n) `mod` n

-- Strengthened Baillie-PSW primality test
-- https://arxiv.org/abs/2006.14425v2
strengthenedBpswTest :: Integer -> Bool
strengthenedBpswTest n =
    -- step 1 and 2
    sprpTest n 2 && case lucasSeqParameter n of
        Just (paramD, paramP, paramQ) ->
            -- step 3
            case testSlprp n paramD paramP paramQ of
                Just (v, q) ->
                    -- step 4 & 5
                    -- Note: v = Vₙ₊₁, q = Q⁽ⁿ⁺¹⁾ᐟ²
                    testVprp v paramQ n && testEulerCriterion paramQ q n
                Nothing -> False
        Nothing -> False

isPrime :: (Integral a, Bits a) => a -> Bool
isPrime n
    | n `elem` [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31] = True
    | any (\x -> n `mod` x == 0) [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31] = False
    | n < 1369 = n > 1 -- 37^2 = 1369
    | n <= 65535 = minFactorTbl ! shiftR (fromIntegral n) 1 == 1 -- (2^16 - 1) = 65535
    | n <= 4294967295 = sprpTestUInt32 (fromIntegral n) (sprpBase (fromIntegral n)) -- (2^32 - 1) = 4294967295
    | otherwise = strengthenedBpswTest (fromIntegral n)
{-# SPECIALIZE isPrime :: Int -> Bool #-}
{-# SPECIALIZE isPrime :: Integer -> Bool #-}

{- FOURMOLU_DISABLE -}
numberToIndex :: Int -> Int
numberToIndex n
    | n < 0 = error "it must not be a negative number"
    | n < 2 = 0
    | otherwise = 48 * (x `div` 210) + idxTbl ! (x `mod` 210)
  where
    x = n - 2
    idxTbl :: UArray Int Int
    idxTbl =
        listArray
            (0, 209)
            [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 2, 2, 2
            , 2, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 6, 6
            , 7, 7, 7, 7, 7, 7, 8, 8, 8, 8, 9, 9, 10, 10, 10
            , 10, 11, 11, 11, 11, 11, 11, 12, 12, 12, 12, 12, 12, 13, 13
            , 14, 14, 14, 14, 14, 14, 15, 15, 15, 15, 16, 16, 17, 17, 17
            , 17, 17, 17, 18, 18, 18, 18, 19, 19, 19, 19, 19, 19, 20, 20
            , 20, 20, 20, 20, 20, 20, 21, 21, 21, 21, 22, 22, 23, 23, 23
            , 23, 24, 24, 25, 25, 25, 25, 26, 26, 26, 26, 26, 26, 26, 26
            , 27, 27, 27, 27, 27, 27, 28, 28, 28, 28, 29, 29, 29, 29, 29
            , 29, 30, 30, 31, 31, 31, 31, 32, 32, 32, 32, 32, 32, 33, 33
            , 34, 34, 34, 34, 34, 34, 35, 35, 35, 35, 35, 35, 36, 36, 36
            , 36, 37, 37, 38, 38, 38, 38, 39, 39, 39, 39, 39, 39, 40, 40
            , 41, 41, 41, 41, 41, 41, 42, 42, 42, 42, 43, 43, 44, 44, 44
            , 44, 45, 45, 46, 46, 46, 46, 46, 46, 46, 46, 46, 46, 47, 47]

indexToNumber :: Int -> Int
indexToNumber idx
    | idx < 0 = error "it must not be a negative number"
    | otherwise = (210 * q) + wheelPrimeCandidates ! r
  where
    q = idx `div` 48
    r = idx `mod` 48
    wheelPrimeCandidates :: UArray Int Int
    wheelPrimeCandidates =
        listArray
            (0, 47)
            [ 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71
            , 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 121, 127, 131, 137, 139, 143
            , 149, 151, 157, 163, 167, 169, 173, 179, 181, 187, 191, 193, 197, 199, 209, 211]
{- FOURMOLU_ENABLE -}

-- Note: This implementation doesn't take overflow into account.
nextPrime :: Int -> Int
nextPrime n
    | n < 2 = 2
    | n < 3 = 3
    | n < 5 = 5
    | n < 7 = 7
    | otherwise =
        let idx =
                if numberToIndex n /= numberToIndex (n + 1)
                    then numberToIndex n + 1
                    else numberToIndex n
         in fromMaybe (error "") $ find isPrime $ map indexToNumber [idx ..]

prevPrime :: Int -> Int
prevPrime n
    | n <= 2 = error "there is no smaller prime"
    | n <= 3 = 2
    | n <= 5 = 3
    | n <= 7 = 5
    | n <= 11 = 7
    | otherwise =
        let idx = numberToIndex n - 1
         in fromMaybe (error "")
                . find isPrime
                . map indexToNumber
                $ iterate (\x -> x - 1) idx
