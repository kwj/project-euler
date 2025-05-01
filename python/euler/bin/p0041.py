# project euler: problem 41

# (sum_{1}_{9} n) mod 3 = 45 mod 3 = 0  --> 9-digit pandigital number is a multiple of 3.
# (sum_{1}_{8} n) mod 3 = 36 mod 3 = 0  --> 8-digit pandigital number is a multiple of 3.
# (sum_{1}_{7} n) mod 3 = 28 mod 3 = 1
# (sum_{1}_{6} n) mod 3 = 21 mod 3 = 0  --> 6-digit pandigital number is a multiple of 3.
# (sum_{1}_{5} n) mod 3 = 15 mod 3 = 0  --> 5-digit pandigital number is a multiple of 3.
# (sum_{1}_{4} n) mod 3 = 10 mod 3 = 1
#
# 2143 is a 4-digit pandigital and is also prime.

from functools import reduce
from itertools import permutations

from euler.lib.prime import is_prime
from euler.lib.util import is_pandigital_nz


# This implementation depends on the permutation lists are emitted in lexicographic ordering
# according to the order of the input *iterable*.
def compute() -> str:
    for digits in [reversed(range(1, 7 + 1)), reversed(range(1, 4 + 1))]:
        for n_lst in permutations(digits):
            n = reduce(lambda x, y: 10 * x + y, n_lst)
            if is_pandigital_nz(n) and is_prime(n):
                return str(n)

    raise RuntimeError('unreachable!')


def solve() -> str:
    return compute()
