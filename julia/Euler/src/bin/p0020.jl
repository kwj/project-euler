
# project euler: problem 20

module Prob0020

function solve_0020(n = 100)
    sum(digits(factorial(big(n))))
end

end #module

using .Prob0020: solve_0020
export solve_0020
