
# project euler: problem 76

'''
  another version of problem 31

  coins: 1, 2, 3, ..., 99
  total: 100
'''

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
    result = compute(list(range(1, 100)), 100)
    elapsed_time = perf_counter() - start

    return (result, "{:f}".format(elapsed_time))
