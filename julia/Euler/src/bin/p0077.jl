
# project euler: problem 77

module Prob0077

import Primes: prime

function prime_generator(c)
    for i in Iterators.countfrom(1)
        put!(c, prime(i))
    end
end

function solve_0077(thr::Int = 5_000)
    p_gen = Channel(prime_generator)
    plst = Array{Int}(undef, 0)
    while true
        push!(plst, take!(p_gen))
        tbl = zeros(Int, length(plst) + 1)
        tbl[1] = 1
        for i in plst
            for j in i:(length(tbl) - 1)
                tbl[j + 1] += tbl[j - i + 1]
            end
        end
        if tbl[end] > thr
            break
        end
    end
    length(plst)
end

end #module

using .Prob0077: solve_0077
export solve_0077
