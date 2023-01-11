
# project euler: problem 15

from math import factorial
from time import perf_counter

def compute(m, n):
    return str(factorial(m + n) // factorial(m) // factorial(n))

def solve():
    start = perf_counter()
    result = compute(20, 20)
    elapsed_time = perf_counter() - start

    return (result, "{:f}".format(elapsed_time))
