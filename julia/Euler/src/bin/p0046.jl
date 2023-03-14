
# project euler: problem 46

module Prob0046

import Primes: isprime

function solve_0046()
    is_twice_square(n) = iseven(n) == true && isqrt(n ÷ 2) ^ 2 == n ÷ 2

    # Two is not odd
    odd_primes = [3, 5, 7, 11, 13, 17, 19, 23, 29, 31]

    # The odd composite numbers less than 35 have been written in the problem statement.
    for x in Iterators.countfrom(35, 2)
        if isprime(x) == true
            push!(odd_primes, x)
        elseif any(is_twice_square(x - p) for p in odd_primes) == false
            return x
        end
    end
end

end #module

using .Prob0046: solve_0046
export solve_0046
