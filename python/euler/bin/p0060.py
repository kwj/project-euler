# project euler: problem 60

# This implementation is slow. The following is a result on Raspberry Pi 4.
#
# [Ubuntu 23.10]
# % python3
# Python 3.11.6 (main, Oct  8 2023, 05:06:43) [GCC 13.2.0] on linux
# Type "help", "copyright", "credits" or "license" for more information.
# >>>
# % ./solve.py 60
# [Problem 60]
# Answer: 26033
# Elapsed time: 7.719348 sec.
#
# [Ubuntu 24.04]
# % python3
# Python 3.12.3 (main, Apr 10 2024, 05:33:47) [GCC 13.2.0] on linux
# Type "help", "copyright", "credits" or "license" for more information.
# >>>
# % ./solve.py 60
# [Problem 60]
# Answer: 26033
# Elapsed time: 10.614437 sec.
#
# [Ubuntu 24.10]
# % python3
# Python 3.12.7 (main, Oct  3 2024, 15:15:22) [GCC 14.2.0] on linux
# Type "help", "copyright", "credits" or "license" for more information.
# >>>
# % ./solve.py 60
# [Problem 60]
# Answer: 26033
# Elapsed time: 17.551342 sec.
#
#
# These results are terrible. What is going on?

import sys
from collections.abc import Iterable, Mapping, Reversible, Sequence

from euler.lib.prime import is_prime, prime_generator
from euler.lib.util import num_of_digits


def get_pairable_primes(x: int, asc_ps: Iterable[int], curr_minsum: int) -> list[int]:
    def fermat_primality_test(n: int) -> bool:
        # Note: 'n' must be an odd number.
        return pow(2, n - 1, n) == 1

    def is_probably_pair(a: int, upper_a: int, b: int, upper_b: int) -> bool:
        return fermat_primality_test(a * upper_b + b) and fermat_primality_test(b * upper_a + a)  # fmt: skip

    upper_x = 10 ** num_of_digits(x)
    upper_p = 10
    result = []
    for p in asc_ps:
        if x + p >= curr_minsum:
            break
        while p > upper_p:
            upper_p *= 10
        if is_probably_pair(x, upper_x, p, upper_p):
            result.append(p)

    return result


def find_cliques(
    p: int, asc_nbrs: Reversible[int], size: int, tbl: Mapping[int, set[int]]
) -> list[list[int]]:
    def is_clique(hd: int, tl: Sequence[int]) -> bool:
        if tl:
            if all(is_prime(int(str(hd) + str(x))) and is_prime(int(str(x) + str(hd))) for x in tl):
                return is_clique(tl[0], tl[1:])
            else:
                return False
        else:
            return True

    def aux(group: list[int], desc_nbrs: Sequence[int], depth: int) -> None:
        if depth == 0:
            if is_clique(p, group):
                result.append([p] + group)
        else:
            for offset in range(0, len(desc_nbrs) - depth + 1):
                if all(desc_nbrs[offset] in tbl[x] for x in group):
                    aux(group + [desc_nbrs[offset]], desc_nbrs[offset + 1 :], depth - 1)

    result: list[list[int]] = []
    aux([], list(reversed(asc_nbrs)), size)

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

        if cliques := find_cliques(prime, nbrs, size, tbl):
            answer = min(answer, min(map(sum, cliques)))

    return str(answer)


def solve() -> str:
    return compute(5)
