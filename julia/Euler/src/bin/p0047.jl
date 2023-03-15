
# project euler: problem 47

module Prob0047

import Primes: factor

function solve_0047(nfactors::Int = 4)
    cnt = 0
    for x in Iterators.countfrom(1)
        if length(factor(Set, x)) != nfactors
            cnt = 0
        elseif cnt == nfactors - 1
            return x - (nfactors - 1)
        else
            cnt += 1
        end
    end
end

end #module

using .Prob0047: solve_0047
export solve_0047
