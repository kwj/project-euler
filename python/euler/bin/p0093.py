# project euler: problem 93

# combination of numberes:
#   nCk = C(n,k) = C(9,4) = 9*8*7*6 / 4*3*2*1 = 126
#
# arithmetic operations (four_ops):
#   commutative:
#     addition: X + Y
#     multiplication: X * Y
#   no-commutative:
#     subtraction:  X - Y, Y - X
#     division: X / Y, Y / X
#
# patterns:
#   A, B, C, D: numbers
#     4! = 24
#
#   [1] ((A op B) op C) op D
#        ^^^^^^^^
#   [2] (A op B) op (C op D)
#       ^^^^^^^^    ^^^^^^^^
#       ^^^^^^^^^^^^^^^^^^^^
#
#   ^^^: The order of the two terms is irrelevant
#        because four_ops() also considers the reverse order.

from fractions import Fraction
from itertools import combinations, count, product


def four_ops(x1: Fraction, x2: Fraction) -> list[Fraction]:
    result = []

    result.append(x1 + x2)
    result.append(x1 * x2)
    result.append(x1 - x2)
    result.append(x2 - x1)
    if x1 != 0:
        result.append(x2 / x1)
    if x2 != 0:
        result.append(x1 / x2)

    return result


def case_1(a: Fraction, b: Fraction, lst: list[Fraction]) -> list[Fraction]:
    result = []
    for ab in four_ops(a, b):
        # c: lst[0], d:lst[1]
        for abc in four_ops(ab, lst[0]):
            result += four_ops(abc, lst[1])
        # c: lst[1], d:lst[0]
        for abc in four_ops(ab, lst[1]):
            result += four_ops(abc, lst[0])

    return result


def case_2(a: Fraction, b: Fraction, lst: list[Fraction]) -> list[Fraction]:
    result = []
    ab = four_ops(a, b)
    cd = four_ops(lst[0], lst[1])

    for tpl in product(ab, cd):
        result += four_ops(tpl[0], tpl[1])

    return result


def make_numbers(tpl: tuple[Fraction, Fraction, Fraction, Fraction]) -> set[int]:
    lst = []
    for i in range(len(tpl)):
        for j in range(i + 1, len(tpl)):
            rest = []
            for k in range(len(tpl)):
                if k != i and k != j:
                    rest.append(tpl[k])

            lst += case_1(tpl[i], tpl[j], rest)
            lst += case_2(tpl[i], tpl[j], rest)

    return set(x.numerator for x in lst if x.denominator == 1)


def get_consecutive_length(tpl: tuple[Fraction, Fraction, Fraction, Fraction]) -> int:
    nums = make_numbers(tpl)
    for cnt in count(1):
        if cnt not in nums:
            break

    return cnt - 1


def compute() -> str:
    max_length = 0
    for tpl in combinations([Fraction(i) for i in range(1, 10)], 4):
        if (tmp := get_consecutive_length(tpl)) > max_length:
            answer = tpl
            max_length = tmp

    return ''.join(str(frac.numerator) for frac in answer)


def solve() -> str:
    return compute()
