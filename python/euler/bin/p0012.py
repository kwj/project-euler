
# project euler: problem 12

from euler.lib.util import num_of_divisors
from time import perf_counter

def compute(limit):
    tri_num = 1
    inc = 2
    while num_of_divisors(tri_num) <= limit:
        tri_num += inc
        inc += 1

    return str(tri_num)

def solve():
    start = perf_counter()
    result = compute(500)
    elapsed_time = perf_counter() - start

    return (result, "{:f}".format(elapsed_time))
