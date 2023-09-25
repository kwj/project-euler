# project euler: problem 10

from euler.lib.prime import get_primes


def compute(num: int) -> str:
    return str(sum(get_primes(num)))


def solve() -> str:
    return compute(2_000_000)
