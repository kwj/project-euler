
# project euler: problem 32

"""
  m * n = mn (multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital)

  - numbers of digits of multiplicand/multiplier must be 4 or less.
  - if number of digits of multiplicand is 4, number of digits of multiplier is 1.
  - if number of digits of multiplicand is 3, number of digits of multiplier is 2.

  multiplicand/multiplier/product : 4-digits/1-digit/4-digits or 3-digits/2-digits/4-digits
"""

from euler.lib.util import is_pandigital_nz
from time import perf_counter

# --> [(9-digits number, prod), ...]
def make_cands():
    p1 = [(m1 * (10 ** 5) + m2 * (10 ** 4) + m1 * m2, m1 * m2)
          for m1 in range(1_000, 10_000) for m2 in range(2, 10) if m1 * m2 < 10_000]
    p2 = [(m1 * (10 ** 6) + m2 * (10 ** 4) + m1 * m2, m1 * m2)
          for m1 in range(100, 1_000) for m2 in range(10, 100) if m1 * m2 < 10_000]

    return p1 + p2

def compute():
    return str(sum(set(prod for n, prod in make_cands() if is_pandigital_nz(n) == True)))

def solve():
    start = perf_counter()
    result = compute()
    elapsed_time = perf_counter() - start

    return (result, "{:f}".format(elapsed_time))
