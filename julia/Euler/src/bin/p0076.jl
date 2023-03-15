
# project euler: problem 76

#=
  another version of problem 31

  coins: 1, 2, 3, ..., 99
  total: 100
=#

module Prob0076

function solve_0076(coins::Vector{Int} = collect(1:99), target::Int = 100)
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

using .Prob0076: solve_0076
export solve_0076
