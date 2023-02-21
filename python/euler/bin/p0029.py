
# project euler: problem 29

from math import isqrt, lcm
from euler.lib.util import flatten, get_max_exp
from time import perf_counter

def compute(upper):
    dup_tbl = [[0 for i in range(upper + 1)] for j in range(upper + 1)]
    for x in range(2, isqrt(upper) + 1):
        for y in range(2, get_max_exp(upper, base=x) + 1):
            for z in range(1, y):
                k = lcm(y, z) // y
                length = len(dup_tbl[x ** y][max(k, 2):(upper * z // y + 1):k])
                dup_tbl[x ** y][max(k, 2):(upper * z // y + 1):k] = [1] * length

    return str((upper - 1) ** 2 - flatten(dup_tbl).count(1))

def solve():
    start = perf_counter()
    result = compute(100)
    elapsed_time = perf_counter() - start

    return (result, "{:f}".format(elapsed_time))
