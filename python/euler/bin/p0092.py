
# project euler: problem 92

'''
  9^2 = 81
  -->
        99 -> 9^2 * 2 = 162
       999 -> 9^2 * 3 = 243
      9999 -> 9^2 * 4 = 324
     99999 -> 9^2 * 5 = 405
    999999 -> 9^2 * 6 = 486
   9999999 -> 9^2 * 7 = 567

  If we know lengths of chain about the range of n <= 567,
  we can find the length of chain quickly if n > 567.

  This problem can be solved by multinomial coefficients
  because the next chain is determined by combination with
  repetition of numeric digit.

    Multinomial Coefficients
    https://en.wikipedia.org/wiki/Multinomial_theorem#Multinomial_coefficients

      n! / (k{1}! * k{2}! * ... * k{n}!)
'''

from euler.lib.util import num_of_digits
from math import factorial
from itertools import combinations_with_replacement
from collections import Counter
from functools import reduce, cache
from operator import mul
from time import perf_counter

@cache
def is_group89(n):
    while n != 89 and n > 1:
        acc = 0
        while n != 0:
            acc += (n % 10) * (n % 10)
            n //= 10
        return is_group89(acc)

    return n == 89

def compute(limit):
    assert limit % 10 == 0 and limit != 0, 'This implementation works correctly only if the limit is a power of 10.'
    ndigits = num_of_digits(limit) - 1

    patterns = combinations_with_replacement((0, 1, 4, 9, 16, 25, 36, 49, 64, 81), r=ndigits)
    denominators = (reduce(mul, k_fact) for k_fact in
                    (map(factorial, Counter(pat).values()) for pat in patterns if is_group89(sum(pat)) == True))
    numerator = factorial(ndigits)

    return str(sum(numerator // d for d in denominators))

def solve():
    start = perf_counter()
    result = compute(10_000_000)
    elapsed_time = perf_counter() - start

    return (result, "{:f}".format(elapsed_time))
