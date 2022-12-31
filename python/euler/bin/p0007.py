
# project euler: problem 7

from itertools import islice
from euler.lib.prime import generator
from time import perf_counter

def compute(num):
    return str(next(islice((n for n in generator()), num - 1, num)))

def solve():
    start = perf_counter()
    result = compute(10_001)
    elapsed_time = perf_counter() - start

    return (result, "{:f}".format(elapsed_time))
