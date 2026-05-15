
# project euler: problem 50

module Prob0050

import Primes: nextprime, isprime

function cumsum_generator(c::Channel)
    acc = 0
    p = 0
    while true
        p = nextprime(p + 1)
        acc += p
        put!(c, acc)
    end
end

function init_cumsum_lst(cs_gen, limit)
    lst = [0]
    while lst[end] < limit
        push!(lst, take!(cs_gen))
    end
    lst
end

function solve_0050(limit::Int = 1_000_000)
    @assert limit >= 3

    # cs_lst: [0, prime(1), prime(1) + prime(2), prime(1) + prime(2) + prime(3), ...]
    # the last element of cs_lst is equal or greater than `limit`
    cs_gen = Channel{Int}(cumsum_generator)
    cs_lst = init_cumsum_lst(cs_gen, limit)

    k = length(cs_lst) - 2
    left = 1
    while true
        diff = cs_lst[left + k] - cs_lst[left]
        if diff >= limit
            left = 1
            k -= 1
        elseif isprime(diff)
            return diff
        else
            left += 1
            if left + k > length(cs_lst)
                push!(cs_lst, take!(cs_gen))
            end
        end
    end

    error("Not Reached")
end

end #module

using .Prob0050: solve_0050
export solve_0050
