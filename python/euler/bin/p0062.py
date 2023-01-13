
# project euler: problem 62

from itertools import count
from time import perf_counter

def compute(n_of_perms):
    def make_key(n):
        return ''.join(sorted(list(str(n))))

    tbl = dict()
    for n in count(1):
        cube = n * n * n
        match (key := make_key(cube)) in tbl:
            case True:
                tbl[key] = tbl[key] + [n]
                if len(tbl[key]) == n_of_perms:
                    return str(tbl[key][0] ** 3)
            case _:
                tbl[key] = [n]

def solve():
    start = perf_counter()
    result = compute(5)
    elapsed_time = perf_counter() - start

    return (result, "{:f}".format(elapsed_time))
