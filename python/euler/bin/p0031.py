
# project euler: problem 31

from time import perf_counter

def compute(coins, target):
    tbl = [0] * (target + 1)
    tbl[0] = 1

    for c in coins:
        for i in range(c, target + 1):
            tbl[i] += tbl[i - c]

    return str(tbl[target])

def solve():
    start = perf_counter()
    result = compute([1, 2, 5, 10, 20, 50, 100, 200], 200)
    elapsed_time = perf_counter() - start

    return (result, "{:f}".format(elapsed_time))
