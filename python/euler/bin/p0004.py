# project euler: problem 4

from euler.lib.util import is_palindrome


def compute(digits: int) -> str:
    assert digits > 0, 'range error'
    n_upper = (10**digits) - 1
    n_lower = 10 ** (digits - 1)
    blk_upper_limit = 10 ** (digits * 2) - 1
    blk_lower_limit = 10 ** ((digits - 1) * 2) if digits > 1 else 0
    blk_width = 10 ** (digits * 2 - 2)
    answer = []

    # The blocks of product of two numbers are checked in descending order
    # The block width, blk_width, is 1/100 of the upper limit
    for blk_lower in reversed(range(blk_lower_limit, blk_upper_limit, blk_width)):
        blk_upper = blk_lower + blk_width - 1
        for x in reversed(range(n_lower, n_upper + 1)):
            if x * x < blk_lower:
                break
            for y in reversed(range(n_lower, min(blk_upper // x, x) + 1)):
                if (tmp := x * y) < blk_lower:
                    break
                elif is_palindrome(tmp):
                    answer.append(tmp)

        if answer:
            return str(max(answer))

    assert False, 'unreachable!'


def solve() -> str:
    return compute(3)  # 3-digit
