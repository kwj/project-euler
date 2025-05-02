# project euler: problem 24

from collections.abc import MutableSequence
from math import factorial


def make_fact_tbl(num: int, depth: int) -> list[int]:
    result = []
    i = num - 1
    divisor = factorial(num - depth)
    for _ in range(depth):
        result.append(factorial(i) // divisor)
        i -= 1

    return result


def compute(nth: int, elm_lst: MutableSequence[int], depth: int) -> str:
    idx = nth - 1
    result = []
    for n in make_fact_tbl(len(elm_lst), depth):
        result.append(elm_lst.pop(idx // n))
        idx %= n

    return ''.join(map(str, result))


def solve() -> str:
    lst = list(range(10))
    return compute(1_000_000, lst, len(lst))
