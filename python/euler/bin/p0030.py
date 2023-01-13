
# project euler: problem 30

"""
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

    # The following function is a little slower than search_from_rhs().
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
"""

from itertools import combinations_with_replacement
from time import perf_counter

# This implementation depends on the combination tuples are emitted in lexicographic ordering
# according to the order of the input *iterable*. Otherwise, we need to use other comparisons.
def search_from_rhs():
    def to_digit_tpl(n):
        def aux(n):
            if n >= 10:
                return aux(n // 10) + [n % 10]
            else:
                return [n]

        return tuple(sorted(aux(n)))

    acc = 0
    pow_tbl = [0, 1, 32, 243, 1024, 3125, 7776, 16807, 32768, 59049]

    # The chain.from_iterable() method is not used for readability here.
    for ndigits in range(2, 7):
        for tpl in combinations_with_replacement([0, 1, 2, 3, 4, 5, 6, 7, 8, 9], ndigits):
            n = sum(map(lambda x: pow_tbl[x], tpl))
            if to_digit_tpl(n) == tpl:
                acc += n

    return acc

def compute():
    return str(search_from_rhs())

def solve():
    start = perf_counter()
    result = compute()
    elapsed_time = perf_counter() - start

    return (result, "{:f}".format(elapsed_time))
