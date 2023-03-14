
# project euler: problem 10

module Prob0010

import Primes: primes

function solve_0010(upper = 2_000_000)
    sum(primes(upper))
end

end #module

using .Prob0010: solve_0010
export solve_0010
