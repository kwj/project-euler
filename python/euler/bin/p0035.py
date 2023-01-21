
# project euler: problem 35

from euler.lib.prime import get_prime_tbl, tbl_to_primes
from time import perf_counter

def compute(limit):
    def check_rot_nums(n):
        s = str(n) * 2
        m = len(s) // 2

        return all(prime_tbl[elm] for elm in (int(s[pos:pos + m]) for pos in range(m)))

    prime_tbl = get_prime_tbl(limit)

    return str(len([n for n in tbl_to_primes(prime_tbl) if check_rot_nums(n)]))

def solve():
    start = perf_counter()
    result = compute(1_000_000)
    elapsed_time = perf_counter() - start

    return (result, "{:f}".format(elapsed_time))
