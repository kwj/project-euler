
# project euler: problem 1

from time import perf_counter

def sum_of_divs_by(n, ulimit):
    upper = ulimit - 1

    return (n + (upper - (upper % n))) * (upper // n) // 2

def compute(ulimit):
    return sum_of_divs_by(3, ulimit) + sum_of_divs_by(5, ulimit) - sum_of_divs_by(15, ulimit)
    
def solve():
    start = perf_counter()
    result = compute(1000)
    elapsed_time = perf_counter() - start

    return (result, "{:f}".format(elapsed_time))
