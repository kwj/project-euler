
# project euler: problem 21

from math import isqrt
from time import perf_counter

def d(n):
    lst = []
    for i in range(1, isqrt(n) + 1):
        if n % i != 0:
            continue
        if i * i == n:
            lst.append(i)
        else:
            lst.append(i)
            lst.append(n//i)

    return sum(lst) - n

def compute(num):
    num_lst = []
    for i in range(2, num):
        if i in num_lst:
            continue
        d1 = d(i)
        d2 = d(d1)
        if i == d2 and d1 != d2:
            num_lst.append(d1)
            num_lst.append(d2)

    return str(sum(num_lst))

def solve():
    start = perf_counter()
    result = compute(10_000)
    elapsed_time = perf_counter() - start

    return (result, "{:f}".format(elapsed_time))
