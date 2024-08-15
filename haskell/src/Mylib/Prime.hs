module Mylib.Prime (
    primeNumbers,
    primes,
    isPrime,
    nextPrime,
    prevPrime,
) where

import Control.Arrow ((&&&))
import Data.Array.Unboxed (UArray, listArray, (!))
import Data.Bits (countTrailingZeros, shiftR, testBit, xor, (.&.))
import Data.List (find, unfoldr)
import Data.Maybe (fromMaybe)
import Data.Word (Word64)

import qualified Data.Numbers.Primes as P (primes)

import Mylib.Factor (minFactorTbl)
import Mylib.Math (getCTZ, isqrt, kronecker)
import Mylib.Util (bitLength, headExn)

primeNumbers :: [Int]
primeNumbers = P.primes

primes :: Int -> Int -> [Int]
primes low high =
    if low <= high
        then
            takeWhile (<= high) $ dropWhile (< low) primeNumbers
        else
            error "range error"

powerModW64 :: Word64 -> Int -> Word64 -> Word64
powerModW64 b e m = powerModW64' b e m 1

powerModW64' :: Word64 -> Int -> Word64 -> Word64 -> Word64
powerModW64' b e m result
    | e == 0 = result
    | testBit e 0 = powerModW64' ((b * b) `mod` m) (shiftR e 1) m ((result * b) `mod` m)
    | otherwise = powerModW64' ((b * b) `mod` m) (shiftR e 1) m result

{- FOURMOLU_DISABLE -}
sprpBase :: Int -> Word64
sprpBase n = bases ! idx
  where
    h = fromIntegral n :: Word64
    h1 = (shiftR h 16 `xor` h) * 0x45d9f3b
    h2 = (shiftR h1 16 `xor` h1) * 0x45d9f3b
    idx = (shiftR h2 16 `xor` h2) .&. 0xff
    bases :: UArray Word64 Word64
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

millerRabinTest :: Int -> Word64 -> Bool
millerRabinTest n base =
    cond1 || cond2
  where
    n' = fromIntegral n
    s = countTrailingZeros (n' - 1)
    d = fromIntegral $ shiftR (n' - 1) s
    cond1 = powerModW64 base d n' == 1
    cond2 = elem (n' - 1) $ map (\k -> powerModW64 base (d * 2 ^ k) n') [0 .. (s - 1)]

lucasTest :: Int -> Bool
lucasTest n = lucasTest' (fromIntegral n)

lucasTest' :: Integer -> Bool
lucasTest' n =
    case lucasSeqParameter n of
        Just (d, _, q) ->
            let (u, v, qk) = lucasSeq n d 1 1 q q delta (bitLength delta - 2)
             in cond1 u || cond2 v qk
        Nothing -> False
  where
    s = getCTZ (n + 1)
    delta = shiftR (n + 1) s
    cond1 u = u == 0
    cond2 v qk =
        elem 0 . take s $
            unfoldr
                (\(x, y) -> Just (x, (((x * x) - 2 * y) `mod` n, (y * y) `mod` n)))
                (v, qk)

-- return: Maybe (D, P, Q)
lucasSeqParameter :: Integer -> Maybe (Integer, Integer, Integer)
lucasSeqParameter n
    | isqrt_n * isqrt_n == n = Nothing
    | d == 5 = Just (d, 5, 5)
    | otherwise = Just (d, 1, (1 - d) `div` 4)
  where
    isqrt_n = isqrt n
    ~d =
        snd
            . headExn
            . dropWhile ((/= -1) . fst)
            . map (flip kronecker n &&& id)
            $ zipWith (*) (iterate (+ 2) 5) (cycle [1, -1])

lucasSeq ::
    Integer ->
    Integer ->
    Integer ->
    Integer ->
    Integer ->
    Integer ->
    Integer ->
    Int ->
    (Integer, Integer, Integer)
lucasSeq n d u v q qk delta x
    | x < 0 = (u, v, qk)
    | shiftR delta x .&. 1 == 0 = lucasSeq n d u' v' q qk' delta (x - 1)
    | otherwise =
        let u'' = u' + v'
            v'' = v' + u' * d
            u''' = shiftR (if u'' .&. 1 /= 0 then u'' + n else u'') 1 `mod` n
            v''' = shiftR (if v'' .&. 1 /= 0 then v'' + n else v'') 1 `mod` n
         in lucasSeq n d u''' v''' q ((qk' * q) `mod` n) delta (x - 1)
  where
    u' = (u * v) `mod` n
    v' = (v * v - 2 * qk) `mod` n
    qk' = (qk * qk) `mod` n

isPrime :: Int -> Bool
isPrime n
    | even n = n == 2
    | n <= 65535 = if n < 2 then False else minFactorTbl ! (shiftR n 1) == 1 -- (2^16 - 1) = 65535
    | mod n 3 == 0 = False
    | mod n 5 == 0 = False
    | mod n 7 == 0 = False
    | mod n 11 == 0 = False
    | mod n 13 == 0 = False
    | mod n 17 == 0 = False
    | mod n 19 == 0 = False
    | mod n 23 == 0 = False
    | mod n 29 == 0 = False
    | mod n 31 == 0 = False
    | n <= 4294967295 = millerRabinTest n (sprpBase n) -- (2^32 - 1) = 4294967295
    | otherwise = millerRabinTest n 2 && lucasTest n

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

nextPrime :: Int -> Int
nextPrime n
    | n < 2 = 2
    | n < 3 = 3
    | n < 5 = 5
    | n < 7 = 7
    | otherwise =
        let idx =
                if numberToIndex n /= numberToIndex (n + 1)
                    then (numberToIndex n) + 1
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
        let idx = (numberToIndex n) - 1
         in fromMaybe (error "")
                . find isPrime
                . map indexToNumber
                $ iterate (\x -> x - 1) idx
