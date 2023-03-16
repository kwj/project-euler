
# project euler: problem 1

module Prob0001

function solve_0001(limit::Int = 1000)
    sum(union(3:3:(limit - 1), 5:5:(limit - 1)))
end

end #module

using .Prob0001: solve_0001
export solve_0001
