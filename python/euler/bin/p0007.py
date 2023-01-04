
# project euler: problem 7

from itertools import islice
from euler.lib.prime import prime_generator
from time import perf_counter

def compute(num):
    return str(next(islice((n for n in prime_generator()), num - 1, num)))

def solve():
    start = perf_counter()
    result = compute(10_001)
    elapsed_time = perf_counter() - start

    return (result, "{:f}".format(elapsed_time))
