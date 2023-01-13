
# project euler: problem 46

from math import isqrt
from itertools import count
from euler.lib.prime import is_prime
from time import perf_counter

def is_twice_squre(n):
    tmp = isqrt(n // 2)

    return n % 2 == 0 and tmp * tmp == n // 2

def compute():
    # Two is not an odd number.
    odd_primes = [3, 5, 7, 11, 13, 17, 19, 23, 29, 31]

    # The odd composite numbers less than 35 have been written in the problem statement.
    for x in count(35, 2):
        if is_prime(x):
            odd_primes.append(x)
        elif any(is_twice_squre(x - p) for p in odd_primes):
            pass
        else:
            return str(x)

def solve():
    start = perf_counter()
    result = compute()
    elapsed_time = perf_counter() - start

    return (result, "{:f}".format(elapsed_time))
