
# project euler: problem 92

#=
  9^2 = 81
  -->
        99 -> 9^2 * 2 = 162
       999 -> 9^2 * 3 = 243
      9999 -> 9^2 * 4 = 324
     99999 -> 9^2 * 5 = 405
    999999 -> 9^2 * 6 = 486
   9999999 -> 9^2 * 7 = 567


  BTW,

  This problem can also be solved by combination and permutation because
  the next chain is determined by combination of numeric digit.

  Once a combination of digit numbers is determined, the total number of
  numbers represented by the combination, i.e., the number of permutations
  of multisets, can be obtained.

    n! / (k{1}! * k{2}! * ... * k{n}!)   [where n = k{1} + k{2} + ... + k{n}]

  For exapmle, we assume that a 4-digit combination with repetition is {1, 2, 2, 3}.

  They can be considered as one group since next chain of all of these
  numbers is 18 (1^2 + 2^2 + 2^2 + 3^2). Note that the final number in
  this chain is 89.

  There are 12 numbers presented by the combination as following.

    1223, 1232, 1322, 2123, 2132, 2213,
    2231, 2312, 2321, 3122, 3212, 3221

  The value of 12 can be obtained from above permutations with repetitions formula:

    {num of digits}! / ({num of '1'}! * {num of '2}! * {num of '3'}!)
      = 4! / (1! * 2! * 1!)
      = 24 / 2
      = 12

  On the other hand, we assume that an another combination with repetition is {1, 2, 3, 3}.
  There are 12 numbers from the combination in the same way.

    1233, 1323, 1332, 2133, 2313, 2331,
    3123, 3132, 3213, 3231, 3312, 3321

  However, the chain from this combination {1, 2, 3, 3} arrives at 1.
  We can therefore ignore the combination.

  note: This method doesn't scale well, haha.

  Note:
    Number of combinations with repetition
    https://en.wikipedia.org/wiki/Combination#Number_of_combinations_with_repetition

    Permutations of multisets
    https://en.wikipedia.org/wiki/Permutation#Permutations_of_multisets
=#

module Prob0092

import Combinatorics: with_replacement_combinations

function is_group89(n::Int)
    while n != 89 && n > 1
        acc = 0
        while n != 0
            acc += (n % 10) ^ 2
            n ÷= 10
        end
        return is_group89(acc)
    end

    return n == 89
end

function countmap(lst::Vector{Int})
    tbl = Dict{Int, Int}()
    for elt in lst
        haskey(tbl, elt) ? tbl[elt] += 1 : tbl[elt] = 1
    end
    tbl
end

function solve_0092(limit::Int = 10_000_000)
    @assert limit % 10 == 0 && limit != 0 "This implementation works correctly only if the limit is a power of 10"

    n_digits = ndigits(limit) - 1
    patterns = with_replacement_combinations([i^2 for i = 0:9], n_digits)
    numerator = factorial(n_digits)
    denominators = [reduce(*, map(factorial, values(countmap(pat)))) for pat in patterns if is_group89(sum(pat)) == true]

    sum(div(numerator, d) for d in denominators)
end

end #module

using .Prob0092: solve_0092
export solve_0092
