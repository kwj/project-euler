
# project euler: problem 29

from time import perf_counter

def brute_force(upper):
    return len({a ** b for a in range(2, upper + 1) for b in range(2, upper + 1)})

def compute(upper):
    return str(brute_force(upper))

def solve():
    start = perf_counter()
    result = compute(100)
    elapsed_time = perf_counter() - start

    return (result, "{:f}".format(elapsed_time))
