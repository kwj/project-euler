# project euler: problem 35

from euler.lib.prime import is_prime, primes


def compute(limit: int) -> str:
    def check_rot_nums(n: int) -> bool:
        s = str(n) * 2
        m = len(s) // 2

        return all(is_prime(elm) for elm in (int(s[pos : pos + m]) for pos in range(m)))

    return str(len([n for n in primes(limit) if check_rot_nums(n) is True]))


def solve() -> str:
    return compute(1_000_000)
