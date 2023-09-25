# project euler: problem 80

from math import isqrt


def compute(limit: int, digit: int) -> str:
    return str(
        sum(
            sum(map(int, str(isqrt(10 ** ((digit - 1) * 2) * n))[:digit]))
            for n in range(1, (limit + 1))
            if not (n**0.5).is_integer()
        )
    )


def solve() -> str:
    return compute(100, 100)
