
# project euler: problem 49

from functools import reduce
from euler.lib.prime import sieve
from time import perf_counter

def get_prime_tbl(ndigits):
    p_tbl = {}

    pt = sieve(10 ** (ndigits - 1), 10 ** ndigits)
    for p in pt.get_primes():
        key = ''.join(sorted(str(p)))
        v = p_tbl.get(key, [])
        v.append(p)
        p_tbl[key] = v

    return p_tbl

def compute(ndigits):
    p_tbl = get_prime_tbl(ndigits)
    for lst in p_tbl.values():
        if (length := len(lst)) < 3:
            continue
        for i in range(length - 2):
            for j in range(i + 1, length - 1):
                tmp = lst[j] * 2 - lst[i]    # (x + a) * 2 - x = x + 2a
                if tmp in lst and lst[i] != 1487 and lst[j] != 4817:
                    return reduce(lambda x, y: x + y, map(str, [lst[i], lst[j], tmp]))

    assert False, 'Not Reached'

def solve():
    start = perf_counter()
    result = compute(4)
    elapsed_time = perf_counter() - start

    return (result, "{:f}".format(elapsed_time))
