
# project euler: problem 5

module Prob0005

function solve_0005(start = 1, stop = 20)
    lcm(start:stop)
end

end #module

using .Prob0005: solve_0005
export solve_0005
