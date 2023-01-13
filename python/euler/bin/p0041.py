
# project euler: problem 41

"""
  (sum_{1}_{9} n) mod 3 = 45 mod 3 = 0  --> 9 digits pandigital number is a multiple of 3, not a prime.
  (sum_{1}_{8} n) mod 3 = 36 mod 3 = 0  --> 8 digits pandigital number is a multiple of 3, not a prime.
  (sum_{1}_{7} n) mod 3 = 28 mod 3 = 1
  (sum_{1}_{6} n) mod 3 = 21 mod 3 = 0  --> 6 digits pandigital number is a multiple of 3, not a prime.
  (sum_{1}_{5} n) mod 3 = 15 mod 3 = 0  --> 5 digits pandigital number is a multiple of 3, not a prime.
  (sum_{1}_{4} n) mod 3 = 10 mod 3 = 1

  2143 is a 4-digit pandigital and is also prime.
"""

from euler.lib.util import is_pandigital_nz
from euler.lib.prime import is_prime
from itertools import permutations
from functools import reduce
from time import perf_counter

# This implementation depends on the permutation lists are emitted in lexicographic ordering
# according to the order of the input *iterable*.
def compute():
    for digits in [range(7, 0, -1), range(4, 0, -1)]:
        for n_lst in permutations(digits):
            n = reduce(lambda x, y: 10 * x + y, n_lst)
            if is_pandigital_nz(n) and is_prime(n):
                return str(n)

    assert False, 'not reached'

def solve():
    start = perf_counter()
    result = compute()
    elapsed_time = perf_counter() - start

    return (result, "{:f}".format(elapsed_time))
