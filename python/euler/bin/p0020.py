# project euler: problem 20

from math import factorial


def compute(num: int) -> str:
    return str(sum(map(int, str(factorial(num)))))


def solve() -> str:
    return compute(100)
