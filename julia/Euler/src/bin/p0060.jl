
# project euler: problem 60

module Prob0060

import Primes: isprime, nextprime

function get_pairable_primes(x, asc_prime_lst, limit)
    function is_pair(x, upper_x, y, upper_y)
        isprime(x * upper_y + y) && isprime(y * upper_x + x)
    end
    upper_x = 10 ^ ndigits(x)
    upper_p = 10
    result::Vector{Int} = []
    for p in asc_prime_lst
        if p > upper_p
            upper_p *= 10
        end
        if x + p < limit && is_pair(x, upper_x, p, upper_p) == true
            push!(result, p)
        end
    end
    reverse(result)
end

function find_cliques(desc_prime_lst, size, tbl)
    function aux(group, ps, depth)
        if depth == 0
            push!(result, group)
        else
            for offset in 1:(length(ps) - depth + 1)
                if isempty(group) || all(x -> in(ps[offset], tbl[x]), group)
                    aux(push!(copy(group), ps[offset]), ps[(offset + 1):end], depth - 1)
                end
            end
        end
    end

    result::Vector{Vector{Int}} = []
    aux(Array{Int,1}(undef,0), desc_prime_lst, size)
    result
end

function solve_0060(size_of_clique::Int = 5)
    @assert size_of_clique > 1 "invalid parameter"
    prime_lst = [[3], [3]]    # Group prime numbers by the remainder divided by 3 (but include 3).
    tbl = Dict{Int, Set{Int}}()
    answer = typemax(Int)

    # start from the 4th prime, 7
    p = 5
    while true
        p = nextprime(p + 1)

        # break this loop when it has verified the answer is smallest
        if p >= answer
            break
        end

        # find all prime numbers smaller than 'p' that can be paired with 'p'
        idx = p % 3
        nbr_lst = get_pairable_primes(p, prime_lst[idx], answer)
        tbl[p] = Set(nbr_lst)
        # update known prime numbers
        push!(prime_lst[idx], p)

        # if number of connectable primes is less than 'size_of_clique - 1', check the next prime.
        if length(nbr_lst) < size_of_clique - 1
            continue
        end

        cliques = find_cliques(nbr_lst, size_of_clique - 1, tbl)
        if isempty(cliques) == false
            answer = min(answer, minimum(map(x -> p + sum(x), cliques)))
        end
    end
    answer
end

end #module

using .Prob0060: solve_0060
export solve_0060
