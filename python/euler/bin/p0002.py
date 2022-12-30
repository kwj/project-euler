
# project euler: problem 2

from itertools import takewhile
from time import perf_counter

def fib_gen():
    a, b = 0, 1
    while True:
        a, b = a + b, a
        yield a

def compute(ulimit):
    return str(sum(i for i in takewhile(lambda n: n < ulimit, fib_gen()) if i % 2 == 0))

def solve():
    start = perf_counter()
    result = compute(4_000_000)
    elapsed_time = perf_counter() - start

    return (result, "{:f}".format(elapsed_time))
