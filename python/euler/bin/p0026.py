
# project euler: problem 26

from functools import reduce
from euler.lib.util import factorize, divisors
from time import perf_counter

def pp(n):
    while n % 2 == 0:
        n //= 2
    while n % 5 == 0:
        n //= 5

    return n

def totient(n):
    return reduce(lambda x, y: x * y, map(lambda x: (x[0] - 1) * (x[0] ** (x[1] - 1)), factorize(n)))

def find_repetend_length(d):
    if (d := pp(d)) == 1:
        return 0

    for k in divisors(totient(d)):
        if pow(10, k, d) == 1:
            return k

    assert False, 'not reached'

def compute(limit):
    max_length = 0
    num = 0
    for i in range(limit - 1, ((limit // 2) - 1), -1):
        if i <= max_length:
            break
        repetend_length = find_repetend_length(i)
        if repetend_length > max_length:
            num = pp(i)
            max_length = repetend_length

    return str(num)

def solve():
    start = perf_counter()
    result = compute(1_000)
    elapsed_time = perf_counter() - start

    return (result, "{:f}".format(elapsed_time))
