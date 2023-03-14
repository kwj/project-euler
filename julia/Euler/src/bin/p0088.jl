
# project euler: problem 88

#=
  N(k) = a1 + a2 + ... + ak = a1 * a2 * ... * ak
    N(k) must be composite numbers.
  min_N(k): minimal product-sum N(k)

  when k = 2
    sum {2,2} = prod {2,2}
  when k > 2 and {a1, a2, a3, ..., ak}
    min sum = sum {1, 1, ..., 1} = k
    --> min_N(k) >= k
  when k > 2 and {a1, a2, a3, ..., ak} = {1, ..., 1, 2, k}
    for all k>2, there exists Ak = {a1, a2, ..., ak} = {1, 1, ..., 1, 2, k}, prod Ak = sum Ak = N(k) = 2k
    --> min_N(k) <= 2k

  2 <= k <= 12000
  --> k <= min_N(k) <= 24000
=#

module Prob0088

import Primes: primes

function solve_0088(limit::Int = 12_000)
#=
    function aux(p::Int, s::Int, len::Int, num::Int)
        len += 1
        #for x in Iterators.countfrom(num)
        for x = num:((limit * 2) ÷ p)
            next_p = p * x
            next_s = s + x
            k = next_p - next_s + len
            if k > limit
                return
            end
            if next_p < tbl[k]
                tbl[k] = next_p
            end
            aux(next_p, next_s, len, num)
        end
    end
=#
    function aux(p::Int, s::Int, len::Int, num::Int)
        # p: product, s: sum
        k = p - s + len
        if k > limit
            return
        end
        if p < tbl[k]
            tbl[k] = p
        end

        next_len = len + 1
        for x = num:((limit * 2) ÷ p)
            aux(p * x, s + x, next_len, x)
        end
    end

    tbl = fill(limit * 2, limit)

    #=
    for x = 2:isqrt(limit * 2)
        aux(x, x, 1, x)
    end
    =#
    aux(1, 0, 0, 2)

    sum(unique(@view tbl[2:end]))
end

end #module

using .Prob0088: solve_0088
export solve_0088
