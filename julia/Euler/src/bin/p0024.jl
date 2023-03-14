
# project euler: problem 24

module Prob0024

function solve_0024(idx = 1_000_000, lst = collect(0:9))
    idx -= 1
    acc = 0
    for i = (length(lst) - 1):-1:0
        (blk, idx) = divrem(idx, factorial(i))
        acc = acc * 10 + lst[blk + 1]
        deleteat!(lst, blk + 1)
    end
    acc
end

end #module

using .Prob0024: solve_0024
export solve_0024
