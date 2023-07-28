
# project euler: problem 21

from euler.lib.util import get_sigma_tbl
from math import isqrt
from time import perf_counter

def compute(num):
    upper = num - 1
    d_tbl = get_sigma_tbl(1, upper)
    for x in range(upper + 1):
        d_tbl[x] -= x

    return str(sum(x + d_tbl[x] for x in range(2, upper + 1) if x > d_tbl[x] and d_tbl[d_tbl[x]] == x))

def solve():
    start = perf_counter()
    result = compute(10_000)
    elapsed_time = perf_counter() - start

    return (result, "{:f}".format(elapsed_time))
