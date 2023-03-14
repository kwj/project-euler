
# project euler: problem 37

#=
  candidate numbers: [2357][1379]*[37] (n >= 10)
=#

module Prob0037

import Primes: isprime

function make_cands()
    primes = [2, 3, 5, 7]
    cands = [2, 3, 5, 7]

    while length(cands) != 0
        cands = [10x + y for (x, y) in Iterators.product(cands, [1, 3, 7, 9]) if isprime(10x + y) == true]
        append!(primes, cands)
    end
    (10x + y for (x, y) in Iterators.product(primes, [3, 7]) if isprime(10x + y) == true)
end

function check_left_truncatable(n)
    d = ndigits(n) - 1
    while d > 0
        if isprime(n % (10 ^ d)) == false
            return false
        end
        d -= 1
    end
    return true
end

function solve_0037()
    primes = [x for x in make_cands() if check_left_truncatable(x) == true]
    @assert length(primes) == 11 "Number of prime numbers is invalid"

    sum(primes)
end

end #module

using .Prob0037: solve_0037
export solve_0037
