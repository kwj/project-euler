
# project euler: problem 51

#=
  the smallest prime which, by replacing part of the number with same digit,
  is part of an eight prime value family

  -> eight numbers out of '0' to '9' are used for replacement

  1) the last digit is not eligible for replacement
    It make some even numbers after replacement.

  2) the number of digits of the prime numbers is greater than number of digits to be replaced
    the reason is since 1).

  3) the number of digits that can be replaced is only a multiples of 3
    if 'n' is not multiples of 3, some replaced numbers will contain multiples of 3.

      number  digits  sum  'mod 3'    [n>0]
      ---------------------------------------------
      0       n       0    0
      1       n       n    n mod 3
      2       n       2n   2n mod 3
      3       n       3n   3n mod 3 = 0
      4       n       4n   4n mod 3 = n mod 3
      5       n       5n   5n mod 3 = 2n mod 3
      6       n       6n   6n mod 3 = 0
      7       n       7n   7n mod 3 = n mod 3
      8       n       8n   8n mod 3 = 2n mod 3
      9       n       9n   9n mod 3 = 0

  4) If prime family size is 8, at least one of 0, 1, or 2 is a number to be replaced.
=#

module Prob0051

import Primes: primes, isprime
import Combinatorics: powerset
import ..Util: undigits

function is_family(p, f_size)
    for n = 0:(10 - f_size)
        p_digits = digits(p)
        for mask in powerset(findall(x -> x == n, p_digits), 3)
            if mask[1] == 1 || length(mask) % 3 != 0
                continue
            end
            cnt = 1
            p_digits_tmp = copy(p_digits)
            for d = (n + 1):9
                p_digits_tmp[mask] .= d
                if isprime(undigits(p_digits_tmp)) == true
                    cnt += 1
                end
                if cnt == f_size
                    return true
                elseif (f_size - cnt) > (9 - d)
                    # size of this prime family is less than 'f_size'
                    break
                end
            end
        end
    end
    return false
end

function solve_0051(f_size::Int = 8)
    first(p for exp in Iterators.countfrom(3) for p in primes(10^exp, 10^(exp+1)) if is_family(p, f_size) == true)
end

end #module

using .Prob0051: solve_0051
export solve_0051
