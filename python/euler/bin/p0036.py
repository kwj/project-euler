
# project euler: problem 36

from time import perf_counter

def is_palindromic(n):
    n_d = str(n)
    n_b = bin(n)[2:]

    return n_d == n_d[::-1] and n_b == n_b[::-1]

def compute(limit):
    return str(sum(i for i in range(1, limit, 2) if is_palindromic(i) == True))

def solve():
    start = perf_counter()
    result = compute(1_000_000)
    elapsed_time = perf_counter() - start

    return (result, "{:f}".format(elapsed_time))
