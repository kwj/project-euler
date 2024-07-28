module Sol.P0087 (compute, solve) where

{-
This implementation is quite slow. The following is a result on my Raspberry Pi 4.
I have no ideas to improve performance so far.

% cabal v2-run pe-solver -- 87
[Problem 87]
Answer: 1097343
Elapsed time: 5.112869 sec.
-}

import qualified Data.IntSet as S (fromList, size)

import Mylib.Prime (primeNumbers)

compute :: Int -> String
compute limit =
    show
        . S.size
        . S.fromList
        . filter (<= limit)
        $ [ x2 + y3 + z4
          | x2 <- sqLst
          , y3 <- cbLst
          , z4 <- quLst
          ]
  where
    pow :: Int -> Int -> Int
    pow b e = b ^ e

    sqLst =
        flip pow 2
            <$> takeWhile (<= floor ((fromIntegral limit) ** (1 / 2 :: Double))) primeNumbers
    cbLst =
        flip pow 3
            <$> takeWhile (<= floor ((fromIntegral limit) ** (1 / 3 :: Double))) primeNumbers
    quLst =
        flip pow 4
            <$> takeWhile (<= floor ((fromIntegral limit) ** (1 / 4 :: Double))) primeNumbers

solve :: String
solve = compute 50_000_000
