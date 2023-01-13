
# project euler: problem 44

"""
P(d) = P(k) - P(j) <==> d(3d-1) = k(3k-1) - j(3j-1) = (k-j)(3(k+j)-1)
"""

from itertools import count
from euler.lib.util import factorize, pfactors_to_divisors, is_pentagonal
from time import perf_counter

# get_divisors(n) returns divisors of n(3n-1) which meet the following requirements:
#  - They are less than 'n'.
#  - They are congruent to 'n' modulo 3.
# note: 'n' and '3n-1' are relatively prime.
def get_divisors(n):
    divisors = pfactors_to_divisors(factorize(n) + factorize(3*n - 1))

    return filter(lambda x: x < n and x % 3 == n % 3, divisors)

# d(3d-1) = (k-j)(3(k+j)-1)
#   lhs: d(3d-1)
#   rhs: (k-j) * (3(k+j)-1) = r1 * r2 [r1=k-j, r2=3(k+j)-1]
#   0 < (k-j) < d, d % 3 == (k-j) % 3
def compute():
    def pent(n):
        return (n * (3 * n - 1)) // 2

    for d in count(4):
        lhs = d * (3 * d - 1)
        for r1 in get_divisors(d):
            r2 = lhs // r1
            if r2 % 3 == 2:
                tmp = (r2 + 1) // 3      # tmp = k+j
                if (r1 + tmp) % 2 == 0:
                    k = (r1 + tmp) // 2
                    j = k - r1
                    if is_pentagonal(pent(k) + pent(j)):
                        return str(lhs // 2)

def solve():
    start = perf_counter()
    result = compute()
    elapsed_time = perf_counter() - start

    return (result, "{:f}".format(elapsed_time))
