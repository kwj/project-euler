
# project euler: problem 80

#=
  when n < 100,

    i <= 10^99 * sqrt(n) < i + 1
   -->
    i^2 <= 10^198 * n < (i + 1)^2

  'i' is the 100-digit number we want.
=#

module Prob0080

function solve_0080(limit::Int = 100, n_digits::Int = 100)
    const_pow_of_10 = big(10) ^ ((n_digits - 1) * 2)
    sum(sum(digits(isqrt(const_pow_of_10 * n))) for n = 1:limit if isqrt(n) ^ 2 != n)
end

end #module

using .Prob0080: solve_0080
export solve_0080
