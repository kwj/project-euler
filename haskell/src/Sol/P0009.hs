module Sol.P0009 (compute, solve) where

{-
a = k(m^2 - n^2), b = k * 2mn, c = k(m^2 + n^2)  [m>n>0, gcd(m,n)=1, m+n is odd]

abc = k^3 * (m^4 - n^4) * 2mn
a + b + c = k * 2m(m+n) = 1000

 -> 'k' and 'm' are divisors to 500 (= 1000/2).
    'm+n' is a divisor to 500/m.
    m(m+n) <= 500 --> m <= isqrt(500), m+n <= 500/m
-}

import Mylib.Math (isqrt)
import Mylib.Util (headExn)

compute :: Int -> String
compute perim =
    show $ (k ^ (3 :: Int)) * (m ^ (4 :: Int) - n ^ (4 :: Int)) * 2 * m * n
  where
    halfPerim = div perim 2
    pairs =
        [ (m', n')
        | m' <- [2 .. isqrt halfPerim]
        , n' <- [(mod m' 2) + 1, (mod m' 2) + 3 .. min (m' - 1) ((div 500 m') - m')]
        , gcd m' n' == 1
        , mod halfPerim m' == 0
        , mod halfPerim (m' + n') == 0
        ]
    -- The problem statement mentions there exists exactly only
    -- one Pythagorean triplet when the perimeter is equal to 1000.
    (m, n) = if length pairs == 1 then headExn pairs else error "no answer found"
    k = div (div halfPerim m) (m + n)

solve :: String
solve = compute 1000
