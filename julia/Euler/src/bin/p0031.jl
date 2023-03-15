
# project euler: problem 31

module Prob0031

function solve_0031(coins::Vector{Int} = [1, 2, 5, 10, 20, 50, 100, 200], target::Int = 200)
    tbl = zeros(Int, target + 1)
    tbl[1] = 1

    for c in coins
        for i in c:target
            tbl[i + 1] += tbl[i - c + 1]
        end
    end
    tbl[end]
end

end #module

using .Prob0031: solve_0031
export solve_0031
