
# project euler: problem 6

module Prob0006

function solve_0006(start::Int = 1, stop::Int = 100)
    sum_of_squares(start, stop) = sum(map(x -> x ^ 2, start:stop))
    square_of_sum(start, stop) = sum(start:stop) ^ 2

    abs(sum_of_squares(start, stop) - square_of_sum(start, stop))
end

end #module

using .Prob0006: solve_0006
export solve_0006
