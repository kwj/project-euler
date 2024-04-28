# project euler: problem 60

# This implementation is slow. The following is a result on Ubuntu 23.10 / Raspberry Pi 4.
#
# % python3
# Python 3.11.6 (main, Oct  8 2023, 05:06:43) [GCC 13.2.0] on linux
# Type "help", "copyright", "credits" or "license" for more information.
# >>>
# % ./solve.py 60
# [Problem 60]
# Answer: 26033
# Elapsed time: 7.719348 sec.
#
# It is easy to find a 5-clique, however, it needs time to confirm its sum is the smallest.

import sys

from euler.lib.prime import is_prime, prime_generator
from euler.lib.util import num_of_digits


def get_pairable_primes(x: int, asc_ps: list[int], limit: int) -> list[int]:
    def fermat_primality_test(n: int) -> bool:
        # Note: 'n' must be an odd number.
        return pow(2, n - 1, n) == 1

    def is_probably_pair(a: int, upper_a: int, b: int, upper_b: int) -> bool:
        return (
            fermat_primality_test(a * upper_b + b) is True
            and fermat_primality_test(b * upper_a + a) is True
        )

    upper_x = 10 ** num_of_digits(x)
    upper_p = 10
    result = []
    for p in asc_ps:
        if x + p >= limit:
            break
        while p > upper_p:
            upper_p *= 10
        if is_probably_pair(x, upper_x, p, upper_p) is True:
            result.append(p)

    return result


def find_cliques(
    p: int, asc_ps: list[int], size: int, tbl: dict[int, set[int]]
) -> list[list[int]]:
    def check_concatenating(hd: int, tl: list[int]) -> bool:
        if len(tl) == 0:
            return True
        else:
            if all(
                is_prime(int(str(hd) + str(x))) and is_prime(int(str(x) + str(hd)))
                for x in tl
            ):
                return check_concatenating(tl[0], tl[1:])
            else:
                return False

    def aux(group: list[int], desc_ps: list[int], depth: int) -> None:
        if depth == 0 and check_concatenating(p, group) is True:
            result.append(group)
        else:
            for offset in range(0, len(desc_ps) - depth + 1):
                if len(group) == 0 or all(desc_ps[offset] in tbl[x] for x in group):
                    aux(group + [desc_ps[offset]], desc_ps[offset + 1 :], depth - 1)

    result: list[list[int]] = []
    aux([], list(reversed(asc_ps)), size)

    return result


def compute(group_size: int) -> str:
    # Skip 2, 3 and 5
    p_gen = prime_generator(7)

    # Group of prime numbers by remainder divided by 3 (except 3).
    prime_groups: list[list[int]] = [[3], [3]]
    tbl: dict[int, set[int]] = {}
    size = group_size - 1
    answer = sys.maxsize

    while (prime := next(p_gen)) < answer:
        grp = (prime % 3) - 1
        tbl[prime] = set(nbrs := get_pairable_primes(prime, prime_groups[grp], answer))
        prime_groups[grp].append(prime)
        if len(nbrs) < size:
            continue

        cliques = find_cliques(prime, nbrs, size, tbl)
        if len(cliques) > 0:
            answer = min(answer, min(map(lambda x: prime + sum(x), cliques)))

    return str(answer)


def solve() -> str:
    return compute(5)
