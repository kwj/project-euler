
# project euler: problem 4

from time import perf_counter

def is_palindrome(num):
    ns = str(num)

    return ns == ns[::-1]

def compute(limit):
    answer = []
    width = 10 ** 4
    for upper in range(limit, 0, -width):
        lower = upper - width
        for x in range(999, 0, -1):
            if x * x < lower:
                break
            for y in range(min(upper // x, x), 0, -1):
                tmp = x * y
                if tmp < lower:
                    break
                if is_palindrome(tmp) == True:
                    answer.append(tmp)

        if len(answer) != 0:
            return max(answer)

def solve():
    start = perf_counter()
    result = compute(10 ** 6)
    elapsed_time = perf_counter() - start

    return (result, "{:f}".format(elapsed_time))
