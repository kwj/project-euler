# project euler: problem 1

from math import lcm


def compute(ulimit: int) -> str:
    def sum_of_multiples(n: int) -> int:
        tmp = (ulimit - 1) // n
        return (1 + tmp) * tmp // 2 * n

    return str(sum_of_multiples(3) + sum_of_multiples(5) - sum_of_multiples(lcm(3, 5)))


def solve() -> str:
    return compute(1000)
