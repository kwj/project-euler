
# project euler: problem 47

from itertools import count
from euler.lib.util import factorize
from time import perf_counter

def compute(nfactors):
    cnt = 0
    for x in count(1):
        if len(factorize(x)) != nfactors:
            cnt = 0
        elif cnt == nfactors - 1:
            return str(x - (nfactors - 1))
        else:
            cnt += 1

def solve():
    start = perf_counter()
    result = compute(4)
    elapsed_time = perf_counter() - start

    return (result, "{:f}".format(elapsed_time))
