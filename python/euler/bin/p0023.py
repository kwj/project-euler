# project euler: problem 23

from euler.lib.util import get_sigma_tbl

ULIMIT = 28123


def get_abndnt_tbl() -> list[int]:
    d_tbl = get_sigma_tbl(1, ULIMIT)
    for x in range(1, ULIMIT + 1):
        d_tbl[x] -= x
    d_tbl[0] = 0

    return [x < d_tbl[x] for x in range(ULIMIT + 1)]


def compute() -> str:
    abndnt_flag = get_abndnt_tbl()
    abndnt_lst = []
    acc = 0
    for i in range(1, ULIMIT + 1):
        if i % 2 == 0 and abndnt_flag[i // 2] is True:
            abndnt_lst.append(i // 2)
        if any(abndnt_flag[i - x] is True for x in abndnt_lst):
            continue
        acc += i

    return str(acc)


def solve() -> str:
    return compute()
