
# project euler: problem 36

from euler.lib.util import is_palindrome
from time import perf_counter

def check_palindorme(n):
    return is_palindrome(n) == True and is_palindrome(n, base=2) == True

def compute(limit):
    return str(sum(i for i in range(1, limit, 2) if check_palindorme(i) == True))

def solve():
    start = perf_counter()
    result = compute(1_000_000)
    elapsed_time = perf_counter() - start

    return (result, "{:f}".format(elapsed_time))
