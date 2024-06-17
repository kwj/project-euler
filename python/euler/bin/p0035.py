# project euler: problem 35

from euler.lib.prime import is_prime, primes
from euler.lib.util import num_of_digits


def compute(limit: int) -> str:
    def is_circular_prime(n: int) -> bool:
        k = num_of_digits(n) - 1
        d = pow(10, k)
        for _ in range(k):
            n = (n % 10) * d + n // 10
            if is_prime(n) is False:
                return False

        return True

    return str(len([n for n in primes(limit) if is_circular_prime(n) is True]))


def solve() -> str:
    return compute(1_000_000)
