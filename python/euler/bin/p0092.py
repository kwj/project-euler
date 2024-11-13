# project euler: problem 92

#   9^2 = 81
#   -->
#         99 -> 9^2 * 2 = 162
#        999 -> 9^2 * 3 = 243
#       9999 -> 9^2 * 4 = 324
#      99999 -> 9^2 * 5 = 405
#     999999 -> 9^2 * 6 = 486
#    9999999 -> 9^2 * 7 = 567
#
#   If we know lengths of chain about the range of n <= 567,
#   we can find the length of chain quickly if n > 567.
#
#   BTW,
#
#   This problem can be solved by combination and permutation because
#   the next chain is determined by combination of numeric digit.
#
#   Once a combination of digit numbers is determined, the total number of
#   numbers represented by the combination, i.e., the number of permutations
#   of multisets, can be obtained.
#
#     n! / (k{1}! * k{2}! * ... * k{n}!)   [where n = k{1} + k{2} + ... + k{n}]
#
#   For exapmle, we assume that a 4-digit combination with repetition is {1, 2, 2, 3}.
#
#   They can be considered as one group since next chain of all of these
#   numbers is 18 (1^2 + 2^2 + 2^2 + 3^2). Note that the final number in
#   this chain is 89.
#
#   There are 12 numbers presented by the combination as following.
#
#     1223, 1232, 1322, 2123, 2132, 2213,
#     2231, 2312, 2321, 3122, 3212, 3221
#
#   The value of 12 can be obtained from above permutations with repetitions formula:
#
#     {num of digits}! / ({num of '1'}! * {num of '2}! * {num of '3'}!)
#       = 4! / (1! * 2! * 1!)
#       = 24 / 2
#       = 12
#
#   On the other hand, we assume that an another combination with repetition is {1, 2, 3, 3}.
#   There are 12 numbers from the combination in the same way.
#
#     1233, 1323, 1332, 2133, 2313, 2331,
#     3123, 3132, 3213, 3231, 3312, 3321
#
#   However, the chain from this combination {1, 2, 3, 3} arrives at 1.
#   We can therefore ignore the combination.
#
#
#   Note:
#     Number of combinations with repetition
#     https://en.wikipedia.org/wiki/Combination#Number_of_combinations_with_repetition
#
#     Permutations of multisets
#     https://en.wikipedia.org/wiki/Permutation#Permutations_of_multisets

from collections import Counter
from functools import cache, reduce
from itertools import combinations_with_replacement
from math import factorial
from operator import mul

from euler.lib.util import num_of_digits


@cache
def is_group89(n: int) -> bool:
    while n != 89 and n > 1:
        acc = 0
        while n != 0:
            acc += (n % 10) * (n % 10)
            n //= 10
        return is_group89(acc)

    return n == 89


def compute(limit: int) -> str:
    assert (
        limit % 10 == 0 and limit != 0
    ), 'This implementation works correctly only if the limit is a power of 10.'
    ndigits = num_of_digits(limit) - 1

    patterns = combinations_with_replacement(
        (0, 1, 4, 9, 16, 25, 36, 49, 64, 81), r=ndigits
    )
    denominators = (
        reduce(mul, k_fact)
        for k_fact in (
            map(factorial, Counter(pat).values())
            for pat in patterns
            if is_group89(sum(pat))
        )
    )
    numerator = factorial(ndigits)

    return str(sum(numerator // d for d in denominators))


def solve() -> str:
    return compute(10_000_000)
