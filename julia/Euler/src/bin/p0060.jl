
# project euler: problem 60

#=

size of clique | primes
---------------+--------------------------------------
       4       | 3, 7, 109, 673
       5       | 13, 5197, 5701, 6733, 8389
       6       | 25819, 29569, 209623, 234781, 422089, 452041


Below are results on Raspberry Pi 4 and Mac mini 2018 (i5-8500B/3GHz).

# Raspberry Pi 4

julia> versioninfo()
Julia Version 1.10.2
Commit bd47eca2c8a (2024-03-01 10:14 UTC)
Build Info:
  Official https://julialang.org/ release
Platform Info:
  OS: Linux (aarch64-linux-gnu)
  CPU: 4 × Cortex-A72
  WORD_SIZE: 64
  LIBM: libopenlibm
  LLVM: libLLVM-15.0.7 (ORCJIT, cortex-a72)
Threads: 1 default, 0 interactive, 1 GC (on 4 virtual cores)

julia> @time solve_0060(5)
  1.770793 seconds (159.93 k allocations: 12.773 MiB)
26033

julia> @time solve_0060(4)
  0.004136 seconds (2.12 k allocations: 151.422 KiB)
792


# Mac mini 2018

julia> versioninfo()
Julia Version 1.10.2
Commit bd47eca2c8a (2024-03-01 10:14 UTC)
Build Info:
  Official https://julialang.org/ release
Platform Info:
  OS: macOS (x86_64-apple-darwin22.4.0)
  CPU: 6 × Intel(R) Core(TM) i5-8500B CPU @ 3.00GHz
  WORD_SIZE: 64
  LIBM: libopenlibm
  LLVM: libLLVM-15.0.7 (ORCJIT, skylake)
Threads: 1 default, 0 interactive, 1 GC (on 6 virtual cores)

julia> @time solve_0060(6)
3443.381419 seconds (565.45 M allocations: 38.210 GiB, 0.12% gc time)
1373922

julia> @time solve_0060(5)
  0.580957 seconds (159.93 k allocations: 12.773 MiB)
26033

julia> @time solve_0060(4)
  0.001300 seconds (2.12 k allocations: 151.422 KiB)
792

=#

module Prob0060

import Primes: isprime, nextprime

function get_pairable_primes(x, asc_prime_lst, limit)
    function is_pair(x, upper_x, y, upper_y)
        isprime(x * upper_y + y) && isprime(y * upper_x + x)
    end
    upper_x = 10 ^ ndigits(x)
    upper_p = 10
    result = Int[]
    for p in asc_prime_lst
        if x + p >= limit
            break
        end
        while p > upper_p
            upper_p *= 10
        end
        if is_pair(x, upper_x, p, upper_p) == true
            push!(result, p)
        end
    end
    result
end

function find_cliques(desc_prime_lst, size, tbl)
    function aux(group, offset, depth)
        if depth == 0
            push!(result, group)
        else
            for idx in offset:(length(desc_prime_lst) - depth + 1)
                if all(x -> in(desc_prime_lst[idx], tbl[x]), group)
                    aux(push!(copy(group), desc_prime_lst[idx]), idx + 1, depth - 1)
                end
            end
        end
    end

    result = Vector{Int}[]
    aux(Array{Int,1}(undef,0), 1, size)
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

        cliques = find_cliques(reverse(nbr_lst), size_of_clique - 1, tbl)
        if isempty(cliques) == false
            answer = min(answer, minimum(map(x -> p + sum(x), cliques)))
        end
    end
    answer
end

end #module

using .Prob0060: solve_0060
export solve_0060
