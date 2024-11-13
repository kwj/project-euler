# project euler: problem 23

from euler.lib.util import aliquot_sum_tbl

ULIMIT = 28123


def get_abndnt_tbl() -> list[int]:
    tbl = aliquot_sum_tbl(ULIMIT)

    return [x < tbl[x] for x in range(len(tbl))]


def compute() -> str:
    abndnt_flag = get_abndnt_tbl()
    abndnt_lst = []
    acc = 0
    for i in range(1, len(abndnt_flag)):
        if i % 2 == 0 and abndnt_flag[i // 2]:
            abndnt_lst.append(i // 2)
        if any(abndnt_flag[i - x] for x in abndnt_lst):
            continue
        acc += i

    return str(acc)


def solve() -> str:
    return compute()
