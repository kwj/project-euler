
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
    cnt = 0
    for m in range(1, 10):
        for n in count(1):
            if pow(pow(10, (n - 1)), (1 / n)) > m:
                break
            cnt += 1

    return str(cnt)

def solve():
    start = perf_counter()
    result = compute()
    elapsed_time = perf_counter() - start

    return (result, "{:f}".format(elapsed_time))
