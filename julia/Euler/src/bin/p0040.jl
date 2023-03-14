
# project euler: problem 40

#=
    0.123456789 | 10111213...979899 | 100101102...997998999  | 100010011002  ...
      ---------   -----------------   ---------------------   -----------------
  len: 1 * 9       2 * 90              3 * 900                 4 * 9000      ...
       1 * 9 * 1   2 * 9 * 10          3 * 9 * 100             4 * 9 * 1000  ...
         --> block_num * 9 * base

  block #1: 1-digit number
  block #2: 2-digits number
  block #3: 3-digits number
    ...
  block #n: n-digits number


  0.123456789 | 10111213...979899 | 100101102...997998999  | 100010011002  ...
                     ^[d15]=2
  example d(15)
    pos = 15 > 1 * 9 * (10 ** 0) = 9
    pos <- 15 - 9 = 6

    pos = 6 <= 2 * 9 * (10 ** 1) = 180
    q <- (6 - 1) / 2 = 2, r <- (6 - 1) % 2 = 1
    num <- 10 ** (2 - 1) + q = 10 + 2 = 12
    d[15] = num / (10 ** (2 - r - 1)) % 10
          = 12 / (10 ** 0) % 10
          = 2
=#

module Prob0040

function d(pos)
    n_digits = 1
    while pos > n_digits * 9 * (10 ^ (n_digits - 1))
        pos -= n_digits * 9 * (10 ^ (n_digits - 1))
        n_digits += 1
    end
    (q, r) = divrem(pos - 1, n_digits)
    num = 10 ^ (n_digits - 1) + q

    (num ÷ (10 ^ (n_digits - r - 1))) % 10
end

function solve_0040()
    d(1) * d(10) * d(100) * d(1_000) * d(10_000) * d(100_000) * d(1_000_000)
end

end #module

using .Prob0040: solve_0040
export solve_0040
