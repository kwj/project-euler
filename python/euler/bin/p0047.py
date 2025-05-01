# project euler: problem 47

from itertools import count

from euler.lib.util import factorize


def compute(nfactors: int) -> str:
    cnt = 0
    for x in count(1):
        if len(factorize(x)) != nfactors:
            cnt = 0
        elif cnt == nfactors - 1:
            return str(x - (nfactors - 1))
        else:
            cnt += 1

    raise RuntimeError('unreachable!')


def solve() -> str:
    return compute(4)
