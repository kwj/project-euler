
# project euler: problem 70

#=
  The answer must be a composite number because prime 'n' is not a permutation of phi(n) = n - 1.

  n = p1^k1 * p2^k2 * p3^k3 * ... * p{r}^k{n}
  -->
    phi(N) = N * (1-1/p1) * (1-1/p2) * (1-1/p3) * ... * (1-1/p{n})
      <-->
    N/phi(N) = (p1/(p1-1)) * (p2/(p2-1)) * (p3/(p3-1)) * ... * (p{n}/(p{n}-1))

  From the problem statement, 87109/phi(87109) = 87109 / 79180 = 1.1001.
  11/10 = 1.1 and 7/6 = 1.666..., so 11 <= prime numbers <= 9_999_999 / 11 = 909090.8181...

  The answer N has the following form (p_i are prime numbers)

    N = p1^k1 * p2^k2 * ... * pn^kn  (N < 10^7, n > 1, 11 <= p1 < p2 < ... < pn, k1>2 when n=1)
=#

module Prob0070

import Primes: prevprime, primes
import DataStructures: PriorityQueue, enqueue!, peek

LIMIT = 10 ^ 7 - 1

function prod(pf_lst::Vector{Tuple{Int, Int}})
    reduce(*, map((tpl) -> tpl[1] ^ tpl[2], pf_lst))
end

function phi(pf_lst::Vector{Tuple{Int, Int}})
    reduce(*, map((tpl) -> (tpl[1] ^ (tpl[2] - 1)) * (tpl[1] - 1), pf_lst))
end

function get_ratio(pf_lst::Vector{Tuple{Int, Int}})
    prod(pf_lst) / phi(pf_lst)
end

function pf_generator(c::Channel, tpl::Tuple{Int, Int})
    # Note:
    #   The internal data 'pf_lst' has the following structure.
    #     [(p_n, e_n), ..., (p2, e2), (p1, e1)]
    #   In contrast, the function 'next' returns its reversed list.
    #     [(p1, e1), (p2, e2), ..., (p_n, e_n)]
    function aux(pf_lst::Vector{Tuple{Int, Int}})
        b, e = pf_lst[1]
        if (tmp = LIMIT ÷ prod(pf_lst)) < b
            return pf_lst
        else
            if (prev_p = prevprime(tmp + 1)) > b
                return vcat([(prev_p, 1)], pf_lst)
            else
                return vcat([(b, e + 1)], pf_lst[2:end])
            end
        end
    end

    pf_lst = (tpl[1] !=  tpl[2]) ? [(tpl[2], 1), (tpl[1], 1)] : [(tpl[1], 2)]

    while true
        b, e = pf_lst[1]
        if length(pf_lst) == 1 && e == 1
            # [(p_1, 1)] ->  go to the next prime smaller than p_1
            break
        end

        result = reverse(pf_lst)
        if e > 1
            # [(p_n, e_n), ...] --> [(p_n, e_n - 1), ...]
            pf_lst[1] = (b, e - 1)
        else
            b_x, e_x = pf_lst[2]
            if (prev_p = prevprime(b - 1)) == b_x
                # [(p_n, 1), (p_{n-1}, e_{n-1}), ...] -> [(p_{n-1}, e_{n-1} + 1), ...]
                pf_lst = aux(vcat([(b_x, e_x + 1)], pf_lst[3:end]))
            else
                # [(p_n, 1), (p_{n-1}, e_{n-1}), ...] -> [(prev_prime(p_{n}), 1); (p_{n-1}, e_{n-1}; ...]
                pf_lst = aux(vcat([(prev_p, 1)], pf_lst[2:end]))
            end
        end
        put!(c, result)
    end
end

function is_perm(x::Int, y::Int)
    sort(digits(x)) == sort(digits(y))
end

function solve_0070()
    # priority queue:
    #   (k, v) = (prime_factors, n/phi(n))
    # initial data for pruning:
    #   n = 87109 = 11 * 7919, phi(87109) = 79180
    pq = PriorityQueue{Vector{Tuple{Int, Int}}, Float64}()
    enqueue!(pq, [(11, 1), (7919, 1)] => (87109 / 79180) )

    for p in reverse(primes(11, isqrt(LIMIT)))
        # pruning: end of search
        if get_ratio([(p, 1)]) > peek(pq)[2]
            break
        end

        for pf_lst in Channel((c) -> pf_generator(c, (p, prevprime(LIMIT ÷ p))))
            # pruning: skip to the next prime smaller than 'p'
            if get_ratio(pf_lst[1:min(length(pf_lst), 2)]) > peek(pq)[2]
                break
            end

            if is_perm(prod(pf_lst), phi(pf_lst)) == true
                enqueue!(pq, pf_lst => get_ratio(pf_lst))
            end
        end
    end
    prod(peek(pq)[1])
end

end #module

using .Prob0070: solve_0070
export solve_0070
