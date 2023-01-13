
# project euler: problem 24

from math import factorial
from time import perf_counter

def make_fact_tbl(num, depth):
    result = []
    i = num - 1
    divisor = factorial(num - depth)
    for _ in range(depth):
        result.append(factorial(i) // divisor)
        i -= 1

    return result

def compute(nth, elm_lst, depth):
    idx = nth - 1
    result = []
    for n in make_fact_tbl(len(elm_lst), depth):
        result.append(elm_lst.pop(idx // n))
        idx %= n

    return ''.join(map(str, result))

def solve():
    start = perf_counter()
    lst = list(range(10))
    result = compute(1_000_000, lst, len(lst))
    elapsed_time = perf_counter() - start

    return (result, "{:f}".format(elapsed_time))
