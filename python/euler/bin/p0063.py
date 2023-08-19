
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
    --> n_{max} = floor(1/(1 - log10(m))) = floor( log_{10/m} 10 )
'''

from math import log
from time import perf_counter

def compute():
    return str(sum(int(log(10, 10 / m)) for m in range(1, 10)))

def solve():
    start = perf_counter()
    result = compute()
    elapsed_time = perf_counter() - start

    return (result, "{:f}".format(elapsed_time))
