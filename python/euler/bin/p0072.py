# project euler: problem 72

# https://mathproblems123.wordpress.com/2018/05/10/sum-of-the-euler-totient-function/

from functools import cache
from math import isqrt


@cache
def sum_phi(num: int) -> int:
    v = num * (num + 1) // 2
    for m in range(2, isqrt(num) + 1):
        v -= sum_phi(num // m)
    for d in range(1, (num // (isqrt(num) + 1) + 1)):
        v -= ((num // d) - (num // (d + 1))) * sum_phi(d)

    return v


def compute(limit: int) -> str:
    return str(sum_phi(limit) - sum_phi(1))


def solve() -> str:
    return compute(1_000_000)
