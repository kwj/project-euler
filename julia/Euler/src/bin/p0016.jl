
# project euler: problem 16

module Prob0016

function solve_0016(exp = 1_000)
    sum(digits(big(2) ^ exp))
end

end #module

using .Prob0016: solve_0016
export solve_0016
