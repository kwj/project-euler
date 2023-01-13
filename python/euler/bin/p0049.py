
# project euler: problem 49

from itertools import combinations
from functools import reduce
from euler.lib.prime import sieve
from time import perf_counter

def compute(limit):
    def is_perm(p1, p2, p3):
        s1 = ''.join(sorted(str(p1)))
        s2 = ''.join(sorted(str(p2)))
        s3 = ''.join(sorted(str(p3)))

        return s1 == s2 and s1 == s3

    result = []
    pt = sieve(1000, limit)
    for i, j in combinations(pt.get_primes(), 2):
        m = (i + j) // 2
        if pt.is_prime(m) == True and is_perm(i, m, j) == True:
            result.append([i, m, j])

    result = list(filter(lambda lst: lst[0] != 1487 and lst[2] != 8147, result))
    assert len(result) == 1, 'too many answers'

    return reduce(lambda x, y: x + y, map(str, result[0]))

def solve():
    start = perf_counter()
    result = compute(9999)
    elapsed_time = perf_counter() - start

    return (result, "{:f}".format(elapsed_time))
