
# project euler: problem 25

from time import perf_counter

def compute(digits):
    limit = pow(10, digits - 1)
    nth = 2
    fib1, fib2 = 1, 1
    while fib2 < limit:
        nth += 1
        fib1, fib2 = fib2, fib2 + fib1

    return str(nth)

def solve():
    start = perf_counter()
    result = compute(1_000)
    elapsed_time = perf_counter() - start

    return (result, "{:f}".format(elapsed_time))
