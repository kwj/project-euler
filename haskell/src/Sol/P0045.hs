module Sol.P0045 (compute, solve) where

{-
Hexagonal numbers are also triangle numbers.
We therefore search for hexagonal numbers which are also pentagonal numbers.

P{i} = H{j}
i(3i - 1) / 2 = j(2j - 1)
3i^2 - i = 4j^2 - 2j
(6i - 1)^2 - 1 = 3(4j - 1)^2 - 3
-->
(6i - 1)^2 - 3(4j - 1)^2 = -2
 ------        ------
    X            Y

see https://imomath.com/index.cgi?page=ntPellsEquationPellType
-->
z0 = 2 + sqrt(3), z = 1 + sqrt(3)
X{n} + Y{n} sqrt(3) = (1 + sqrt(3)) (2 + sqrt(3))^n

X{n} = 2X{n-1} + 3Y{n-1}
Y{n} = X{n-1} + 2Y{n-1}
  where X{0} = 1, Y{0} = 1
-}

gpeSols :: [(Int, Int)]
gpeSols = iterate (\(x, y) -> (2 * x + 3 * y, x + 2 * y)) (1, 1)

compute :: Int -> String
compute nth =
    show $ j * ((2 * j) - 1)
  where
    z = snd $ filter (\(x, y) -> mod x 6 == 5 && mod y 4 == 3) gpeSols !! (nth - 1)
    j = (z + 1) `div` 4

solve :: String
solve = compute 3
