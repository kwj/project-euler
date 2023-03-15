
# project euler: problem 12

#=
  triangle number's formula is n(n + 1)/2 and 'n' and 'n + 1' are coprime.
  Therefore, ...
    - 'n/2' and 'n+1' are coprime (when 'n' is even)
    - 'n' and '(n+1)/2' are coprime (when 'n' is odd)

  assume that f(n) returns number of divisors of 'n'.
  f(a*b) = f(a) * f(b) when 'a' and 'b' are coprime.
=#

module Prob0012

import Primes: factor

function num_of_divs(n)
    prod(map(x -> x + 1, values(factor(Dict, n))))
end

function solve_0012(thr::Int = 500)
    n = 1
    while true
        if num_of_divs(n) * num_of_divs((n + 1) ÷ 2) > thr
            break
        end
        n += 1
        if num_of_divs(n ÷ 2) * num_of_divs(n + 1) > thr
            break
        end
        n += 1
    end
    (n * (n + 1)) ÷ 2
end

end #module

using .Prob0012: solve_0012
export solve_0012
