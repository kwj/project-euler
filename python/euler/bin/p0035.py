# project euler: problem 35

from euler.lib.prime import get_prime_tbl, tbl_to_primes


def compute(limit: int) -> str:
    def check_rot_nums(n: int) -> bool:
        s = str(n) * 2
        m = len(s) // 2

        return all(
            prime_tbl[elm] for elm in (int(s[pos : pos + m]) for pos in range(m))
        )

    prime_tbl = get_prime_tbl(limit)

    return str(len([n for n in tbl_to_primes(prime_tbl) if check_rot_nums(n) is True]))


def solve() -> str:
    return compute(1_000_000)
