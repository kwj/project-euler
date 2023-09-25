# project euler: problem 49

from functools import reduce

from euler.lib.prime import sieve


def get_prime_tbl(ndigits: int) -> dict[str, list[int]]:
    p_tbl: dict[str, list[int]] = {}

    pt = sieve(10 ** (ndigits - 1), 10**ndigits)
    for p in pt.get_primes():
        key = ''.join(sorted(str(p)))
        v = p_tbl.get(key, [])
        v.append(p)
        p_tbl[key] = v

    return p_tbl


def compute(ndigits: int) -> str:
    p_tbl = get_prime_tbl(ndigits)
    for lst in p_tbl.values():
        if (length := len(lst)) < 3:
            continue
        for i in range(length - 2):
            for j in range(i + 1, length - 1):
                tmp = lst[j] * 2 - lst[i]  # (x + a) * 2 - x = x + 2a
                if tmp in lst and lst[i] != 1487 and lst[j] != 4817:
                    return reduce(lambda x, y: x + y, map(str, [lst[i], lst[j], tmp]))

    assert False, 'unreachable!'


def solve() -> str:
    return compute(4)
