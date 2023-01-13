
# project euler: problem 38

"""
  It is clear that number X is within 4 digits from the problem statement.

    1) if number X is four digits, n = 2  (X * 1 -> 4-digits, X * 2 -> 5-digits)
    2) if number X is three digits, n = 3  (X * 1 -> 3-digits, X * 2 -> 3-digits, X * 3 -> 3-digits)
    3) if number X is two digits, n = 4  (X * 1 -> 2-digits, X * 2 -> 2-digits, X * 3 -> 2-digits, X * 4 -> 3-digits)
    4) if number X is one digit, n = 9 or 5 (only X=1 and n=9, X=9 and n=5).

  case #1:
    5000 <= X <= 9999
  case #2:
    100 <= X <= 333
  case #3:
    10 <= X <= 33
  case #4:
    X = 1, 9
"""

from functools import reduce
from time import perf_counter

def compute():
    lst = []
    for m, spec in [(2, (5000, 10000)), (3, (100, 334)), (4, (10, 34)), (5, (9, 10)), (9, (1, 2))]:
        for x in range(*spec):
            s = reduce(lambda x, y: x + y, (str(x * k) for k in range(1, m + 1)))
            if ''.join(sorted(s)) == '123456789':
                lst.append(s)
    lst.sort(reverse=True)

    return lst[0]

def solve():
    start = perf_counter()
    result = compute()
    elapsed_time = perf_counter() - start

    return (result, "{:f}".format(elapsed_time))
