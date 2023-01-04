
# project euler: problem 10

from euler.lib.prime import get_primes
from time import perf_counter

def compute(num):
    return str(sum(get_primes(num)))

def solve():
    start = perf_counter()
    result = compute(2_000_000)
    elapsed_time = perf_counter() - start

    return (result, "{:f}".format(elapsed_time))
