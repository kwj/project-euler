
# project euler: problem 63

'''
  n - 1 <= log10(m^n) < n    [m>0, n>0]
    --> n - 1 <= n * log10(m) < n
    --> m < 10
   and
    --> (n - 1)/n <= log10(m)
    --> 10 ^ (n - 1)/n <= 10 ^ log10(m)
    --> log10(10 ^ (n - 1)/n) <= log10(m)
    --> 10 ^ (n - 1)/n <= m
'''

from itertools import count
from time import perf_counter

def compute():
    acc = 0
    cnt = 0
    m = 1
    n = 1
    while m < 10:
        while 10 ** ((n - 1) / n) <= m:
            n += 1
            cnt += 1

        m += 1
        acc += cnt

    return str(acc)

def solve():
    start = perf_counter()
    result = compute()
    elapsed_time = perf_counter() - start

    return (result, "{:f}".format(elapsed_time))
