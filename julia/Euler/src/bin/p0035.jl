
# project euler: problem 35

module Prob0035

import Primes: primes, isprime

function solve_0035(limit::Int = 1_000_000)
    function check_rot_nums(n)
        s = string(n, n)
        m = ndigits(n)

        all(isprime(elm) for elm in (parse(Int, s[i:i + m - 1]) for i = 1:m))
    end

    count(check_rot_nums, primes(limit))
end

end #module

using .Prob0035: solve_0035
export solve_0035
