
# project euler: problem 20

from math import factorial
from time import perf_counter

def compute(num):
    return str(sum(map(int, str(factorial(num)))))

def solve():
    start = perf_counter()
    result = compute(100)
    elapsed_time = perf_counter() - start

    return (result, "{:f}".format(elapsed_time))
