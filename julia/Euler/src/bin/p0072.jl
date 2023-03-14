
# project euler: problem 72

#=
  https://mathproblems123.wordpress.com/2018/05/10/sum-of-the-euler-totient-function/
=#

module Prob0072

function sum_phi(num::Int, cache::Dict{Int, Int})
    get!(cache, num) do
        v = num * (num + 1) ÷ 2
        for m = 2:isqrt(num)
            v -= sum_phi(num ÷ m, cache)
        end
        for d = 1:(num ÷ (isqrt(num) + 1))
            v -= ((num ÷ d) - (num ÷ (d + 1))) * sum_phi(d, cache)
        end
        v
    end
end

function solve_0072(limit::Int = 1_000_000)
    cache = Dict{Int, Int}()
    sum_phi(limit, cache) - sum_phi(1, cache)
end

end #module

using .Prob0072: solve_0072
export solve_0072
