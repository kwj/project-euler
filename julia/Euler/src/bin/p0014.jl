
# project euler: problem 14

module Prob0014

memo = Dict{Int, Int}(1 => 1)

function get_collatz_length(n)
    get!(memo, n) do
        if iseven(n)
            get_collatz_length(n ÷ 2) + 1
        else
            get_collatz_length(3n + 1) + 1
        end
    end
end

function solve_0014(limit::Int = 1_000_000)
    argmax(get_collatz_length, (limit ÷ 2):(limit - 1))
end

end #module

using .Prob0014: solve_0014
export solve_0014
