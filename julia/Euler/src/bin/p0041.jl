
# project euler: problem 41

#=
  (sum_{1}_{9} n) mod 3 = 45 mod 3 = 0  --> 9 digits pandigital number is a multiple of 3, not a prime.
  (sum_{1}_{8} n) mod 3 = 36 mod 3 = 0  --> 8 digits pandigital number is a multiple of 3, not a prime.
  (sum_{1}_{7} n) mod 3 = 28 mod 3 = 1
  (sum_{1}_{6} n) mod 3 = 21 mod 3 = 0  --> 6 digits pandigital number is a multiple of 3, not a prime.
  (sum_{1}_{5} n) mod 3 = 15 mod 3 = 0  --> 5 digits pandigital number is a multiple of 3, not a prime.
  (sum_{1}_{4} n) mod 3 = 10 mod 3 = 1

  2143 is a 4-digit pandigital and is also prime, so we only need to search 7-digits and 4-digits numbers.
=#

module Prob0041

import Primes: isprime
import Combinatorics: permutations
import ..Util: is_pandigital_nz

# This implementation depends on the permutation lists are emitted in lexicographic ordering
# according to the order of the input *array*.
function solve_0041()
    for tpl in [reverse(1:7), reverse(1:4)]
        for n_lst in permutations(tpl)
            n = reduce((x, y) -> 10x + y, n_lst)
            if is_pandigital_nz(n) == true && isprime(n) == true
                return n
            end
        end
    end

    error("Not Reached")
end

end #module

using .Prob0041: solve_0041
export solve_0041
