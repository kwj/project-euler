
# project euler: problem 50

module Prob0050

import Primes: prime, isprime

function cumsum_generator(c::Channel)
    acc = 0
    for nth in Iterators.countfrom(1)
        acc += prime(nth)
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
    # cs_lst: [0, prime(1), prime(1) + prime(2), prime(1) + prime(2) + prime(3), ...]
    cs_gen = Channel(cumsum_generator)
    cs_lst = init_cumsum_lst(cs_gen, limit)

    answer = 0
    i = 1
    consec_length = 0
    while cs_lst[i + consec_length] - cs_lst[i] < limit
        start = cs_lst[i]
        lst = collect(Iterators.dropwhile(p -> p - start >= limit || isprime(p - start) == false, @view cs_lst[end:-1:(i + consec_length)]))
        if length(lst) > 0
            consec_length += length(lst) - 1
            answer = lst[1] - start
        end
        push!(cs_lst, take!(cs_gen))
        i += 1
    end
    answer
end

end #module

using .Prob0050: solve_0050
export solve_0050
