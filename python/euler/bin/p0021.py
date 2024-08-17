# project euler: problem 21

from euler.lib.util import aliquot_sum_tbl


def compute(num: int) -> str:
    upper = num - 1
    tbl = aliquot_sum_tbl(upper)

    return str(
        sum(x + tbl[x] for x in range(2, len(tbl)) if x > tbl[x] and tbl[tbl[x]] == x)
    )


def solve() -> str:
    return compute(10_000)
