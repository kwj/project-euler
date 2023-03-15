
# project euler: problem 30

#=
  (1) 10 ** (n-1) <= n-digits number < 10 ** n
  (2) assume that x is sum of the fifth power of each digit on n-digits number
        n * (1**5) <= x <= n * (9**5) = n * 54049

     when n=6:
       6 * 54049 = 324294
     when n=7
       7 * 54049 = 378343 < 10 ** (7-1) = 1000000 (minimum 7-digits number)
       --> contradiction

  It's clear that 'x' is not a single digit number.
  We need to search 'x' in the follwing range:
    10 <= 'x' <= 354294 = 6 * (9 ** 5)

  We have two approaches to solve this problem. One is to search from left hand side,
  and the other is to search from right hand side.

  1) search from LHS

    # an example in Python
    def search_from_lhs():
        limit = 354_294
        memo_tbl = [0, 1, 32, 243, 1024, 3125, 7776, 16807, 32768, 59049] + [0] * (limit + 1 - 10)
        acc = 0
        for n in range(10, limit + 1):
            memo_tbl[n] = memo_tbl[n // 10] + memo_tbl[n % 10]
            if n == memo_tbl[n]:
                acc += n

        return acc

  2) search from RHS
    We search from combinations of numbers.
=#

module Prob0030

import Combinatorics: with_replacement_combinations

function get_max_ndigits(exp)
    k = 9 ^ exp
    x = 2
    while x * k > 10 ^ (x - 1)
        x += 1
    end
    x - 1
end

# This implementation depends on the combination tuples are emitted in lexicographic ordering
# according to the order of the input *array*. Otherwise, we need to use other algorithm.
function solve_0030(exp::Int = 5)
    pow_tbl = map(x -> x ^ exp, 0:9)
    acc = 0

    for num_of_digits = 2:get_max_ndigits(exp)
        for tpl in with_replacement_combinations(0:9, num_of_digits)
            tmp = sum(map(x -> pow_tbl[x + 1], tpl))
            if sort(digits(tmp)) == tpl
                acc += tmp
            end
        end
    end
    acc
end

end #module

using .Prob0030: solve_0030
export solve_0030
