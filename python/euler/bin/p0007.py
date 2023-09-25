# project euler: problem 7

from itertools import islice

from euler.lib.prime import prime_generator


def compute(num: int) -> str:
    return str(next(islice((n for n in prime_generator()), num - 1, num)))


def solve() -> str:
    return compute(10_001)
