# project euler: problem 45

# H_{n} = T_{2n-1}

from itertools import count

from euler.lib.util import is_pentagonal


def compute() -> str:
    for n in count(144):
        hex_num = n * (2 * n - 1)
        if is_pentagonal(hex_num):
            return str(hex_num)

    assert False, 'unreachable!'


def solve() -> str:
    return compute()
