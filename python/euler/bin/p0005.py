
# project euler: problem 5

from time import perf_counter
from math import lcm

def compute(upper):
    return str(lcm(*list(range(1, upper + 1))))

def solve():
    start = perf_counter()
    result = compute(20)
    elapsed_time = perf_counter() - start

    return (result, "{:f}".format(elapsed_time))
