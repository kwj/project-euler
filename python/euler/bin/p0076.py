# project euler: problem 76

#   another version of problem 31
#
#   coins: 1, 2, 3, ..., 99
#   total: 100

from collections.abc import Iterable


def compute(coins: Iterable[int], target: int) -> str:
    tbl = [0] * (target + 1)
    tbl[0] = 1

    for c in coins:
        for i in range(c, target + 1):
            tbl[i] += tbl[i - c]

    return str(tbl[target])


def solve() -> str:
    return compute(list(range(1, 100)), 100)
