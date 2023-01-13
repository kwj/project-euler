
# project euler: problem 55

from time import perf_counter

def compute():
    cnt = 0
    for i in range(10_000):
        for _ in range(50):
            i += int(str(i)[::-1])
            s = str(i)
            if s == s[::-1]:
                break
        else:
            cnt += 1

    return str(cnt)

def solve():
    start = perf_counter()
    result = compute()
    elapsed_time = perf_counter() - start

    return (result, "{:f}".format(elapsed_time))
