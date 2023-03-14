
# project euler: problem 32

#=
  m * n = mn (multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital)

  - numbers of digits of multiplicand/multiplier must be 4 or less.
  - if number of digits of multiplicand is 4, number of digits of multiplier is 1.
  - if number of digits of multiplicand is 3, number of digits of multiplier is 2.

  multiplicand/multiplier/product : 4-digits/1-digit/4-digits or 3-digits/2-digits/4-digits
=#

module Prob0032

import ..Util: is_pandigital_nz

function solve_0032()
    p1 = [(x * (10 ^ 5) + y * (10 ^ 4) + x * y, x * y) for x = 1_000:9_999 for y = 2:9 if x * y < 10_000]
    p2 = [(x * (10 ^ 6) + y * (10 ^ 4) + x * y, x * y) for x = 100:999 for y = 10:99 if x * y < 10_000]

    sum(unique(prod for (n, prod) in vcat(p1, p2) if is_pandigital_nz(n) == true))
end

end #module

using .Prob0032: solve_0032
export solve_0032
