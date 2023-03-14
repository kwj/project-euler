
# project euler: problem 7

module Prob0007

import Primes: prime

function solve_0007(nth = 10_001)
    prime(nth)
end

end #module

using .Prob0007: solve_0007
export solve_0007
