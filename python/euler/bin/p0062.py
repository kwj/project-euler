# project euler: problem 62

from itertools import count


def compute(n_of_perms: int) -> str:
    def make_key(n: int) -> str:
        return ''.join(sorted(list(str(n))))

    tbl: dict[str, list[int]] = dict()
    for n in count(1):
        cube = n * n * n
        if (key := make_key(cube)) in tbl:
            tbl[key] = tbl[key] + [n]
            if len(tbl[key]) == n_of_perms:
                return str(tbl[key][0] ** 3)
        else:
            tbl[key] = [n]

    raise RuntimeError('unreachable!')


def solve() -> str:
    return compute(5)
