# project euler: problem 46

from itertools import count
from math import isqrt

from euler.lib.prime import is_prime


def is_twice_squre(n: int) -> bool:
    tmp = isqrt(n // 2)

    return n % 2 == 0 and tmp * tmp == n // 2


def compute() -> str:
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

    assert False, 'unreachable!'


def solve() -> str:
    return compute()
