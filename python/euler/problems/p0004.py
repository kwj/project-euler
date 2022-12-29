
# project euler: problem 4

from time import perf_counter

def is_palindrome(num):
    ns = str(num)

    return ns == ns[::-1]

def compute():
    for num in range(999*999, 100*100 - 1, -1):
        if is_palindrome(num) == False:
            continue
        for x in range(999, 100, -1):
            if num % x == 0:
                d = num // x
                if d > 99 and d < 1000:
                    return num

def solve():
    start = perf_counter()
    result = compute()
    elapsed_time = perf_counter() - start

    return (result, "{:f}".format(elapsed_time))
