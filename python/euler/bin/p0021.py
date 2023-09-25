# project euler: problem 21

from euler.lib.util import get_sigma_tbl


def compute(num: int) -> str:
    upper = num - 1
    d_tbl = get_sigma_tbl(1, upper)
    for x in range(upper + 1):
        d_tbl[x] -= x

    return str(
        sum(
            x + d_tbl[x]
            for x in range(2, upper + 1)
            if x > d_tbl[x] and d_tbl[d_tbl[x]] == x
        )
    )


def solve() -> str:
    return compute(10_000)
