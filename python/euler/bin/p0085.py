
# project euler: problem 85

'''
  nCr = n! / ((n-r)! * r!)

      1  2       n-1  n
    +--+--+-- ... --+--+
   1|  |  |   ...   |  |
    +--+--+-- ... --+--+
   2|  |  |   ...   |  |
    +--+--+-- ... --+--+ num of horizontal lines = m + 1
   3|  |  |   ...   |  |
    +--+--+-- ... --+--+
    ....................
    +--+--+-- ... --+--+
   m|  |  |   ...   |  |
    +--+--+-- ... --+--+
      num of vertical lines = n + 1

  (m+1)C2 * (n+1)C2 = m(m+1)/2 * n(n+1)/2 (\approx) 2_000_000
  --> m(m+1)*n(n+1) (\approx) 8_000_000
'''

from euler.lib.util import HeapQueue
from math import isqrt
from itertools import count
from time import perf_counter

def get_diff(m, target):
    def lhs(m, n):
        return m * (m + 1) * n * (n + 1)

    n = isqrt(target // (m * (m + 1))) - 1
    while lhs(m, n) < target:
        n += 1

    if m >= n:
        return None

    if abs(target - lhs(m, n - 1)) < abs(target - lhs(m, n)):
        return abs(target - lhs(m, n - 1)), n - 1
    else:
        return abs(target - lhs(m, n)), n

def compute(target):
    pq = HeapQueue()
    new_target = target * 4
    for m in count(1):
        if (tpl := get_diff(m, new_target)) == None:
            break
        pq.insert((tpl[0], m * tpl[1]))

    return str(pq.peek()[1])

def solve():
    start = perf_counter()
    result = compute(2_000_000)
    elapsed_time = perf_counter() - start

    return (result, "{:f}".format(elapsed_time))
