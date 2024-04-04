# project euler: problem 10

from euler.lib.prime import primes


def compute(num: int) -> str:
    return str(sum(primes(num)))


def solve() -> str:
    return compute(2_000_000)
