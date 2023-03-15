
# project euler: problem 60

module Prob0060

import Primes: isprime, prime
import Combinatorics: combinations

function is_pair(x, y)
    function concat(a, b)
        n = 10
        while b > n
            n *= 10
        end
        a * n + b
    end
    isprime(concat(x, y)) && isprime(concat(y, x))
end

function find_nbrs(prime, prime_lst, limit)
    filter(p -> p + prime < limit && is_pair(prime, p) == true, prime_lst)
end

function is_clique(prime_grp, tbl)
    for idx = 1:(length(prime_grp) - 1)
        if all(x -> in(prime_grp[idx], tbl[x]), prime_grp[idx + 1:end]) == false
            return false
        end
    end
    return true
end

function solve_0060(size_of_clique::Int = 5)
    @assert size_of_clique > 1 "invalid parameter"
    prime_lst = [[3], [3]]    # grouping by mod(prime, 3)  [excluding 3]
    tbl = Dict{Int, Vector{Int}}()
    answer = typemax(Int)

    # start from prime(4) = 7
    for nth in Iterators.countfrom(4)
        p = prime(nth)

        # break this loop when it has verified the answer is smallest
        if p >= answer
            break
        end

        # find all prime numbers smaller than 'p' that can be paired with 'p'
        idx = p % 3
        nbr_lst = find_nbrs(p, prime_lst[idx], answer)
        tbl[p] = nbr_lst
        # update known prime numbers
        push!(prime_lst[idx], p)

        # if number of connectable primes is larger or equal 4, check if sets of five primes satisfy the condition or not.
        if length(nbr_lst) < size_of_clique - 1
            continue
        end
        for prime_grp in combinations(nbr_lst, size_of_clique - 1)
            tmp = sum(prime_grp) + p
            if tmp >= answer
                continue
            end
            if is_clique(prime_grp, tbl) == true
                answer = min(tmp, answer)
            end
        end
    end
    answer
end

end #module

using .Prob0060: solve_0060
export solve_0060
