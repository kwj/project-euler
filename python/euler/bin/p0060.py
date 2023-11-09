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
# Elapsed time: 24.747444 sec.
#
# It is easy to find a 5-clique, however, it needs time to confirm its sum is the smallest.

import sys

from euler.lib.prime import is_probably_prime, prime_generator
from euler.lib.util import num_of_digits


def get_pairable_primes(x: int, asc_ps: list[int], limit: int) -> list[int]:
    def is_prime_pair(a: int, upper_a: int, b: int, upper_b: int) -> bool:
        return is_probably_prime(a * upper_b + b) and is_probably_prime(b * upper_a + a)

    upper_x = 10 ** num_of_digits(x)
    upper_p = 10
    result = []
    for p in asc_ps:
        if p > upper_p:
            upper_p *= 10

        if x + p < limit and is_prime_pair(x, upper_x, p, upper_p):
            result.append(p)

    result.reverse()

    return result


def find_cliques(
    desc_ps: list[int], size: int, tbl: dict[int, set[int]]
) -> list[list[int]]:
    def aux(group: list[int], ps: list[int], depth: int) -> None:
        if depth == 0:
            result.append(group)
        else:
            for offset in range(0, len(ps) - depth + 1):
                if len(group) == 0 or all(ps[offset] in tbl[x] for x in group):
                    aux(group + [ps[offset]], ps[offset + 1 :], depth - 1)

    result = []
    aux([], desc_ps, size)

    return result


def compute(group_size: int) -> str:
    # discard 2, 3 and 5
    p_gen = prime_generator()
    _ = next(p_gen)
    _ = next(p_gen)
    _ = next(p_gen)

    # Grouping by modulus of 3, but exclude 3. Note: prime_groups[0] isn't used.
    prime_groups: list[list[int]] = [
        [],
        [3],
        [3],
    ]
    tbl: dict[int, set[int]] = {3: set()}
    size = group_size - 1
    answer = sys.maxsize

    while (prime := next(p_gen)) < answer:
        grp = prime % 3
        tbl[prime] = set(nbrs := get_pairable_primes(prime, prime_groups[grp], answer))
        prime_groups[grp].append(prime)
        if len(nbrs) < size:
            continue

        cliques = find_cliques(nbrs, size, tbl)
        if len(cliques) > 0:
            answer = min(answer, min(map(lambda x: prime + sum(x), cliques)))

    return str(answer)


def solve() -> str:
    return compute(5)
