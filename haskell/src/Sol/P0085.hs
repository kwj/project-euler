module Sol.P0085 (compute, solve) where

{-
  nCr = n! / ((n-r)! * r!)
      1  2       n-1  n
    +--+--+-- ... --+--+
   1|  |  |   ...   |  |
    +--+--+-- ... --+--+
   2|  |  |   ...   |  |
    +--+--+-- ... --+--+ num of horizontal lines = m + 1
   3|  |  |   ...   |  |
    +--+--+-- ... --+--+
    ....................
    +--+--+-- ... --+--+
   m|  |  |   ...   |  |
    +--+--+-- ... --+--+
      num of vertical lines = n + 1

  (m+1)C2 * (n+1)C2 = m(m+1)/2 * n(n+1)/2 (\approx) 2_000_000
  --> m(m+1)*n(n+1) (\approx) (2_000_000 * 4)
-}

import Data.Function (on)
import Data.List (minimumBy)
import Data.Maybe (fromJust)

import Mylib.Math (isqrt)
import Mylib.Util (headExn)

getApproximation :: Int -> Int -> Maybe (Int, Int)
getApproximation m target
    | m >= n =
        Nothing
    | otherwise =
        let diff_1 = abs (target - lhs m (n - 1))
            diff_2 = abs (target - lhs m n)
         in if diff_1 < diff_2
                then Just (diff_1, m * (n - 1))
                else Just (diff_2, m * n)
  where
    n =
        headExn $
            dropWhile
                (\x -> target - lhs m x > 0)
                [isqrt (target `div` (m * (m + 1))) - 1 ..]

    lhs :: Int -> Int -> Int
    lhs x y = x * (x + 1) * y * (y + 1)

compute :: Int -> String
compute target =
    show
        . snd
        . minimumBy (compare `on` fst)
        . map fromJust
        . takeWhile (/= Nothing)
        $ (flip getApproximation (target * 4)) <$> [1 ..]

solve :: String
solve = compute 2_000_000
