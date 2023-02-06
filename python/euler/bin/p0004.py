
# project euler: problem 4

from euler.lib.util import is_palindrome
from time import perf_counter

def compute(digits):
    assert digits > 0, 'range error'
    n_upper = (10 ** digits) - 1
    n_lower = (10 ** (digits - 1)) - 1
    blk_upper_limit = 10 ** (digits * 2)
    blk_lower_limit = 10 ** ((digits - 1) * 2) if digits > 1 else 0
    blk_width = 10 ** (digits * 2 - 2)
    answer = []

    # The blocks of product of two numbers are checked in descending order
    # The block width, blk_width, is 1/100 of the upper limit
    for blk_upper in range(blk_upper_limit, blk_lower_limit, -blk_width):
        blk_lower = blk_upper - blk_width
        for x in range(n_upper, n_lower, -1):
            if x * x < blk_lower:
                break
            for y in range(min(blk_upper // x, x), n_lower, -1):
                tmp = x * y
                if tmp < blk_lower:
                    break
                if is_palindrome(tmp) == True:
                    answer.append(tmp)

        if len(answer) != 0:
            return str(max(answer))

def solve():
    start = perf_counter()
    result = compute(3)    # 3: 3-digit
    elapsed_time = perf_counter() - start

    return (result, "{:f}".format(elapsed_time))
