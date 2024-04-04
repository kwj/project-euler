# project euler: problem 87

#   >>> 50000000 ** (1/2)
#   7071.067811865475
#   >>> 50000000 ** (1/3)
#   368.40314986403854
#   >>> 50000000 ** (1/4)
#   84.08964152537145

from itertools import takewhile
from math import floor, isqrt

from euler.lib.prime import primes


def compute(limit: int) -> str:
    sq_plst = primes(isqrt(limit - 2**3 - 2**4))
    cb_plst = list(takewhile(lambda n: n <= floor(limit ** (1 / 3)), sq_plst))
    fth_plst = list(takewhile(lambda n: n <= floor(limit ** (1 / 4)), sq_plst))

    result = []
    for z in fth_plst:
        for y in cb_plst:
            for x in sq_plst:
                if (tmp := x**2 + y**3 + z**4) < limit:
                    result.append(tmp)

    return str(len(set(result)))


def solve() -> str:
    return compute(50_000_000)
