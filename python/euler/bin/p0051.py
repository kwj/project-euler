# project euler: problem 51

# the smallest prime which, by replacing part of the number with same digit,
# is part of an eight prime value family
#
#  -> eight numbers out of '0' to '9' are used for replacement
#
# 1) the last digit is not eligible for replacement
#    It make some even numbers after replacement.
#
# 2) the number of digits of the prime numbers is greater than number of digits to be replaced
#    the reason is since 1).
#
# 3) the number of digits that can be replaced is only a multiples of 3
#    if 'n' is not multiples of 3, some replaced numbers will contain multiples of 3.
#
#    number  digits  sum  'mod 3'    [n>0]
#    ---------------------------------------------
#    0       n       0    0
#    1       n       n    n mod 3
#    2       n       2n   2n mod 3
#    3       n       3n   3n mod 3 = 0
#    4       n       4n   4n mod 3 = n mod 3
#    5       n       5n   5n mod 3 = 2n mod 3
#    6       n       6n   6n mod 3 = 0
#    7       n       7n   7n mod 3 = n mod 3
#    8       n       8n   8n mod 3 = 2n mod 3
#    9       n       9n   9n mod 3 = 0
#
# I want to use prime number tables to speed up and simplify. For the
# moment, I assume that prime numbers less than one million. The reason
# is that I want to only consider the case of replacing three digits.
#
# 4) There are at least same three numbers other than last digit.

from collections.abc import Iterable, Sequence
from functools import reduce
from itertools import combinations, count

from euler.lib.prime import is_prime


# pattern:
#   (num_of_digits_to_replace, [d1, ..., dn])
#      1: digit to replace
#      0: digit not to replace
#   note: d1: Least Significant Digit (LSD)
#         dn: Most Significant Digit (MSD)
#
# >>> make_patterns(4)
# [(3, [0, 1, 1, 1])]
# >>> make_patterns(5)
# [(3, [0, 1, 1, 1, 0]), (3, [0, 1, 1, 0, 1]), (3, [0, 1, 0, 1, 1]), (3, [0, 0, 1, 1, 1])]
# >>> make_patterns(6)
# [(3, [0, 1, 1, 1, 0, 0]), (3, [0, 1, 1, 0, 1, 0]), (3, [0, 1, 1, 0, 0, 1]),
#  (3, [0, 1, 0, 1, 1, 0]), (3, [0, 1, 0, 1, 0, 1]), (3, [0, 1, 0, 0, 1, 1]),
#  (3, [0, 0, 1, 1, 1, 0]), (3, [0, 0, 1, 1, 0, 1]), (3, [0, 0, 1, 0, 1, 1]),
#  (3, [0, 0, 0, 1, 1, 1])]
# >>> make_patterns(7)
# [(3, [0, 1, 1, 1, 0, 0, 0]), (3, [0, 1, 1, 0, 1, 0, 0]), (3, [0, 1, 1, 0, 0, 1, 0]),
#  (3, [0, 1, 1, 0, 0, 0, 1]), (3, [0, 1, 0, 1, 1, 0, 0]), (3, [0, 1, 0, 1, 0, 1, 0]),
#  (3, [0, 1, 0, 1, 0, 0, 1]), (3, [0, 1, 0, 0, 1, 1, 0]), (3, [0, 1, 0, 0, 1, 0, 1]),
#  (3, [0, 1, 0, 0, 0, 1, 1]), (3, [0, 0, 1, 1, 1, 0, 0]), (3, [0, 0, 1, 1, 0, 1, 0]),
#  (3, [0, 0, 1, 1, 0, 0, 1]), (3, [0, 0, 1, 0, 1, 1, 0]), (3, [0, 0, 1, 0, 1, 0, 1]),
#  (3, [0, 0, 1, 0, 0, 1, 1]), (3, [0, 0, 0, 1, 1, 1, 0]), (3, [0, 0, 0, 1, 1, 0, 1]),
#  (3, [0, 0, 0, 1, 0, 1, 1]), (3, [0, 0, 0, 0, 1, 1, 1]), (6, [0, 1, 1, 1, 1, 1, 1])]
#
def make_patterns(ndigits: int) -> list[tuple[int, list[int]]]:
    def select_pos() -> list[tuple[int, tuple[int, ...]]]:
        result = []
        positions = list(range(1, ndigits))
        for n in range(3, ndigits, 3):
            result += list(map(lambda x: (n, x), combinations(positions, n)))

        return result

    def flip_bits(pos_tpl: tuple[int, tuple[int, ...]]) -> tuple[int, list[int]]:
        result = [0] * ndigits
        for idx in pos_tpl[1]:
            result[idx] = 1

        return (pos_tpl[0], result)

    return list(map(flip_bits, select_pos()))


# assemble_num(231, [0, 1, 1, 1, 0, 0], 7)
#   --> 237771
def assemble_num(i: int, pat: Iterable[int], r: int) -> int:
    lst = []
    for flag in pat:
        if flag == 0:
            lst.append(i % 10)
            i //= 10
        else:
            lst.append(r)

    return reduce(lambda x, y: 10 * x + y, reversed(lst))


def is_probable(i: int, pat: Sequence[int], current_min: int) -> bool:
    # Is MSB target digit to replace or not?
    if pat[-1] == 1:
        return assemble_num(i, pat, 1) < current_min
    else:
        return assemble_num(i, pat, 0) < current_min


# Check all cases
def find_prime(i: int, pat: Sequence[int]) -> int | None:
    # If MSB is target digit to replace, '0' is not applicable.
    start = 0
    if pat[-1] == 1:
        start = 1
    lst = []
    for r in range(start, 10):
        if is_prime(tmp := assemble_num(i, pat, r)):
            lst.append(tmp)

    # if there are eight primes by this pattern, return the smallest one.
    if len(lst) != 8:
        return None
    else:
        return lst[0]


def compute() -> str:
    for ndigits in count(4):
        answer = 10**ndigits
        pat_tpls = make_patterns(ndigits)

        # n: number of digits to replace (3, 6, ...)
        for n in range(3, ndigits, 3):
            # 'i' must be odd number
            for i in range(10 ** (ndigits - n - 1) + 1, 10 ** (ndigits - n), 2):
                _, pat_lst = zip(*filter(lambda tpl: tpl[0] == n, pat_tpls), strict=True)
                for pat in pat_lst:
                    if (
                        is_probable(i, pat, answer)
                        and (tmp := find_prime(i, pat)) is not None
                        and tmp < answer
                    ):
                        answer = tmp

        if answer != 10**ndigits:
            return str(answer)

    raise RuntimeError('unreachable!')


def solve() -> str:
    return compute()
