
# project euler: problem 52

from itertools import count
from time import perf_counter

def check_num(n):
    def get_key(num):
        return ''.join(sorted(str(num)))

    key_id = get_key(n)
    for mult in range(2, 7):
        if key_id != get_key(n * mult):
            return False

    return True

def compute():
    for ndigits in count(1):
        lower = 10 ** (ndigits - 1)
        upper = (10 ** ndigits) // 6
        for n in range(lower, upper + 1):
            if check_num(n) == True:
                return str(n)

def solve():
    start = perf_counter()
    result = compute()
    elapsed_time = perf_counter() - start

    return (result, "{:f}".format(elapsed_time))
