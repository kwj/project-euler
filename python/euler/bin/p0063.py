
# project euler: problem 63

'''
  n - 1 <= log10(m^n) < n    [m>0, n>0]
    --> n - 1 <= n * log10(m) < n
    --> m < 10
   and
    --> (n - 1)/n <= log10(m)
'''

from math import log
from time import perf_counter

def compute():
    acc = 0
    cnt = 0
    m = 1
    n = 1
    while m < 10:
        upper_m = log(m, 10)
        while (n - 1) / n <= upper_m:
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
