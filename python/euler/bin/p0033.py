
# project euler: problem 33

"""
  case #1: (10a + c) / (10c + b) = a / b
  case #2: (10a + c) / (10b + c) = a / b
  case #3: (10c + a) / (10c + b) = a / b
  case #4: (10c + a) / (10b + c) = a / b
    // prerequisite: a < b

  #1:
      10ab + bc = 10ac + ab
   -> 9ab = 10ac - bc
   -> 9ab - 9ac = ac - bc
   -> 9a(b - c) = c(a - b)
   -> 9a(c - b) = c(b - a)  [a < b => a < b < c]
  #2:
      10ab + bc = 10ab + ac
   -> bc = ac
   -> b = a   ==> NG (contradiction)
  #3:
      10bc + ab = 10ac + ab
   -> 10bc = 10ac
   -> b = a   ==> NG (contradiction)
  #4:
      10bc + ab = 10ab + ac
   -> 10bc - ac = 9ab
   -> bc - ac = 9ab - 9bc
   -> c(b - a) = 9b(a - c)  [a < b => c < a < b]
   -> a - c = c/9 - ac/9b => 1   ==> NG (bacause c/9 < 1)

  I only need to search in case #1.
"""

from math import gcd
from itertools import combinations
from functools import reduce
from time import perf_counter

def make_cands():
    # Note: The function check() assumes that a < b < c.
    #
    # The combination tuples which are generated by itertools.combinations() are emitted
    # in lexicographic ordering according to the order of the input iterable.
    def check(a, b, c):
        return 9 * a * (c - b) == c * (b - a)

    return ((a, b) for a, b, c in combinations(range(1, 10), 3) if check(a, b, c) == True)

def compute():
    a, b = reduce(lambda x, y: (x[0] * y[0], x[1] * y[1]), make_cands())

    return str(b // gcd(a, b))

def solve():
    start = perf_counter()
    result = compute()
    elapsed_time = perf_counter() - start

    return (result, "{:f}".format(elapsed_time))