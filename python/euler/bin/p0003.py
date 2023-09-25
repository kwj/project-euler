# project euler: problem 3

from euler.lib.util import factorize


def compute(num: int) -> str:
    return str(factorize(num)[-1][0])  # [(b1,e1), (b2,e2), ...]  (b{i}<b{j} when i<j)


def solve() -> str:
    return compute(600851475143)
