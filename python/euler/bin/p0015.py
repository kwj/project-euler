# project euler: problem 15

from math import factorial


def compute(m: int, n: int) -> str:
    return str(factorial(m + n) // factorial(m) // factorial(n))


def solve() -> str:
    return compute(20, 20)
