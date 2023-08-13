
# project euler: problem 63

'''
  n - 1 <= log10(m^n) < n    [m>0, n>0]
    --> n - 1 <= n * log10(m) < n
    --> m < 10
   and
    --> (n - 1)/n <= log10(m)
    --> n/n - (n -1)/n >= 1 - log10(m)
    --> 1/n >= 1 - log10(m)
    --> 1/(1 - log10(m)) >= n
'''

from math import log10
from time import perf_counter

def compute():
    cnt = 0
    m = 1
    while m < 10:
        cnt += int(1 / (1 - log10(m)))
        m += 1

    return str(cnt)

def solve():
    start = perf_counter()
    result = compute()
    elapsed_time = perf_counter() - start

    return (result, "{:f}".format(elapsed_time))
