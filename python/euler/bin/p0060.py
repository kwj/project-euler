# project euler: problem 60

# This implementation is slow. The following is a result on Raspberry Pi 4.
#
# % python3
# Python 3.11.2 (main, Mar 13 2023, 12:18:29) [GCC 12.2.0] on linux
# Type "help", "copyright", "credits" or "license" for more information.
# >>>
# % ./solve.py 60
# [Problem 60]
# Answer: 26033
# Elapsed time: 45.732660 sec.
#
# It is easy to find a 5-clique, however, it needs time to confirm its sum is the smallest.

import sys
from itertools import combinations

from euler.lib.prime import is_probably_prime, prime_generator


def is_pair(x: int, y: int) -> bool:
    def concat_num(a: int, b: int) -> int:
        n = 10
        while b > n:
            n = n * 10
        return a * n + b

    return is_probably_prime(concat_num(x, y)) and is_probably_prime(concat_num(y, x))


def find_nbrs(prime: int, prime_set: set[int], limit: int) -> set[int]:
    return {x for x in prime_set if x + prime < limit and is_pair(x, prime) is True}


def is_clique(lst: list[int], tbl: dict[int, set[int]]) -> bool:
    for idx in range(len(lst) - 1):
        if tbl[lst[idx]] >= set(lst[idx + 1 :]):
            continue
        else:
            return False
    return True


def compute() -> str:
    # discard 2, 3 and 5
    p_gen = prime_generator()
    _ = next(p_gen)
    _ = next(p_gen)
    _ = next(p_gen)

    tbl: dict[int, set[int]] = {3: set()}
    prime_set: list[set[int]] = [
        set(),
        {3},
        {3},
    ]  # Grouping by modulus of 3, but exclude 3. prime_set[0] isn't used.
    answer = sys.maxsize

    while (prime := next(p_gen)) < answer - 792:  # 792 = sum([3, 7, 109, 673])
        grp = prime % 3
        tbl[prime] = (nbr_set := find_nbrs(prime, prime_set[grp], answer))
        prime_set[grp].add(prime)
        if len(nbr_set) < 4:
            continue

        for prime_grp in combinations(sorted(list(nbr_set), reverse=True), 4):
            if prime + sum(prime_grp) > answer:
                continue
            if is_clique(list(prime_grp), tbl) is True:
                answer = min(prime + sum(prime_grp), answer)

    return str(answer)


def solve() -> str:
    return compute()
