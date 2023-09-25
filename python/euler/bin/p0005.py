# project euler: problem 5

from math import lcm


def compute(upper: int) -> str:
    return str(lcm(*list(range(1, upper + 1))))


def solve() -> str:
    return compute(20)
