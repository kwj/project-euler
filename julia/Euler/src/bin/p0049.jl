
# project euler: problem 49

module Prob0049

import Primes: primes
import ..Util: undigits

function get_prime_tbl(n_digits)
    p_tbl = Dict{Int, Vector{Int}}()

    for p in primes(10 ^ (n_digits - 1), 10 ^ n_digits)
        key = undigits(sort(digits(p)))
        p_tbl[key] = push!(get(() -> Vector{Int}(), p_tbl, key), p)
    end
    p_tbl
end

function solve_0049(n_digits = 4)
    p_tbl = get_prime_tbl(n_digits)
    for lst in values(p_tbl)
        len = length(lst)
        if len < 3
            continue
        end
        sort!(lst)
        for i = 1:(len-2), j = (i+1):(len-1)
            tmp = lst[j] * 2 - lst[i]
            if tmp ∈ lst && lst[i] != 1487
                return foldl((acc, x) -> acc * (10 ^ n_digits) + x, [lst[i], lst[j], tmp]; init = 0)
            end
        end
    end

    @assert false "not reached"
end

end #module

using .Prob0049: solve_0049
export solve_0049
