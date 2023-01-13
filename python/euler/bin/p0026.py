
# project euler: problem 26

from time import perf_counter

def div_loop(a, b):
    rems = dict()
    cnt = 0
    while True:
        if a == 0:
            return 0
        if a in rems:
            return (cnt - rems[a])
        else:
            rems[a] = cnt
            a = (a * 10) % b
            cnt += 1

def compute(limit):
    max_cycle = 0
    num = 0
    for i in range(limit - 1, 1, -1):
        if i <= max_cycle:
            break
        cycle_len = div_loop(1, i)
        if cycle_len > max_cycle:
            num = i
            max_cycle = cycle_len

    return str(num)

def solve():
    start = perf_counter()
    result = compute(1_000)
    elapsed_time = perf_counter() - start

    return (result, "{:f}".format(elapsed_time))
