
# project euler: problem 3

module Prob0003

import Primes: factor

function solve_0003(n = 600851475143)
    factor(Vector, n)[end]
end

end #module

using .Prob0003: solve_0003
export solve_0003
