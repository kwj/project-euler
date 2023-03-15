
# project euler: problem 15

module Prob0015

function solve_0015(row::Int = 20, col::Int = 20)
    # factorial(big(row + col)) ÷ factorial(row) ÷ factorial(col)
    binomial(row + col, row)
end

end #module

using .Prob0015: solve_0015
export solve_0015
