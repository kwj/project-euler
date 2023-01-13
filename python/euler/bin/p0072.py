
# project euler: problem 72

# https://mathproblems123.wordpress.com/2018/05/10/sum-of-the-euler-totient-function/

from math import isqrt
from functools import cache
from time import perf_counter

@cache
def sum_phi(num):
    v = num * (num + 1) // 2
    for m in range(2, isqrt(num) + 1):
        v -= sum_phi(num // m)
    for d in range(1, (num // (isqrt(num) + 1) + 1)):
        v -= ((num // d) - (num // (d + 1))) * sum_phi(d)

    return v

def compute(limit):
    return str(sum_phi(limit) - sum_phi(1))

def solve():
    start = perf_counter()
    result = compute(1_000_000)
    elapsed_time = perf_counter() - start

    return (result, "{:f}".format(elapsed_time))
