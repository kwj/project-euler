
# project euler: problem 3

from euler.lib.util import factorize
from time import perf_counter

def compute(num):
    return factorize(num)[-1][0]    # [(b1,e1), (b2,e2), ...]  (b{i}<b{j} when i<j)

def solve():
    start = perf_counter()
    result = compute(600851475143)
    elapsed_time = perf_counter() - start

    return (result, "{:f}".format(elapsed_time))
