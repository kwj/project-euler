
from math import isqrt
from enum import Enum
from random import randrange
from itertools import islice

def is_prime(num):
    if num == 2:
        return True
    elif num < 2:
        return False
    elif num % 2 == 0:
        return False
    else:
        for x in range(3, isqrt(num) + 1, 2):
            if num % x == 0:
                return False
        return True

NumType = Enum('NumType', ['PRIME', 'COMPOSITE', 'UNDECIDED'])

def _is_prime_64bit(d, s, n):
    """
    Deterministic variants of the Miller-Rabin primality test (n <= 2 ** 64)
    http://miller-rabin.appspot.com/
    """
    def distinguish(a, d, s, n):
        x = pow(a, d, n)
        if x == 0:
            return NumType.PRIME
        elif x == 1 or x == n - 1:
            return NumType.UNDECIDED
        else:
            for i in range(s):
                x = pow(x, 2, n)
                if x == 0:
                    return NumType.PRIME
                elif x == n - 1:
                    return NumType.UNDECIDED

            return NumType.COMPOSITE

    for a in [2, 325, 9375, 28178, 450775, 9780504, 1795265022]:
        result = distinguish(a, d, s, n)
        if result == NumType.PRIME:
            return True
        elif result == NumType.COMPOSITE:
            return False

    return True

def _is_prime_more_64bit(d, s, n):
    def distinguish(a, d, s, n):
        x = pow(a, d, n)
        if x == 1 or x == n - 1:
            return NumType.UNDECIDED
        else:
            for i in range(s):
                x = pow(x, 2, n)
                if x == n - 1:
                    return NumType.UNDECIDED

            return NumType.COMPOSITE

    def rand_gen():
        while True:
            yield randrange(43, 2**64)

    for a in [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41] + list(islice(rand_gen(), 20)):
        result = distinguish(a, d, s, n)
        if result == NumType.COMPOSITE:
            return False

    return True    # probably prime

def is_probably_prime(num):
    if num < 2:
        return False

    d = num - 1
    s = 0
    while d % 2 == 0:
        d, s = (d // 2), (s + 1)

    if num <= 2 ** 64:
        return _is_prime_64bit(d, s, num)
    else:
        return _is_prime_more_64bit(d, s, num)
