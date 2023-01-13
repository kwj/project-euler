
# project euler: problem 80

from time import perf_counter
from math import isqrt

def compute(limit, digit):
    return str(sum(sum(map(int, list(str(isqrt(10**((digit - 1) * 2) * n))[:digit]))) for n in range(1,(limit + 1)) if not (n ** 0.5).is_integer()))

def solve():
    start = perf_counter()
    result = compute(100, 100)
    elapsed_time = perf_counter() - start

    return (result, "{:f}".format(elapsed_time))
