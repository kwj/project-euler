
# project euler: problem 4

from time import perf_counter

def is_palindrome(num):
    ns = str(num)

    return ns == ns[::-1]

def compute(digits):
    n_upper = (10 ** digits) - 1
    n_lower = (10 ** (digits - 1)) - 1
    limit = 10 ** (digits * 2)
    blk_width = 10 ** (digits * 2 - 2)
    answer = []

    for blk_upper in range(limit, 0, -blk_width):
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
            return max(answer)

def solve():
    start = perf_counter()
    result = compute(3)
    elapsed_time = perf_counter() - start

    return (result, "{:f}".format(elapsed_time))
