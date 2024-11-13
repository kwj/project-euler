# project euler: problem 29

from math import isqrt, lcm

from euler.lib.util import get_max_exp


def make_dupctr_tbl(upper: int) -> list[int]:
    max_exp = get_max_exp(upper, base=2)
    dup_ctr = [0] * (max_exp + 1)

    for x in range(2, max_exp + 1):
        dups = [0] * (upper + 1)
        for y in range(1, x):
            k = lcm(x, y) // x
            length = len(range(max(k, 2), (upper * y // x) + 1, k))
            dups[max(k, 2) : (upper * y // x) + 1 : k] = [1] * length
        dup_ctr[x] = sum(dups)

    return dup_ctr


def compute(upper: int) -> str:
    dup_ctr = make_dupctr_tbl(upper)
    base_limit = isqrt(upper)
    skip_flag = [False] * (base_limit + 1)

    ans = (upper - 1) ** 2
    for b in range(2, base_limit + 1):
        if skip_flag[b]:
            continue
        for e in range(2, get_max_exp(upper, base=b) + 1):
            ans -= dup_ctr[e]
            if (tmp := b**e) <= base_limit:
                skip_flag[tmp] = True

    return str(ans)


def solve() -> str:
    return compute(100)
