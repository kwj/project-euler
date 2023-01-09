
# project euler: problem 14

from time import perf_counter

def compute(limit):
    cache = [0] * limit
    cache[1] = 1

    for cur in range(limit - 1, 1, -1):
        if cache[cur] != 0:
            continue

        path = []
        while cur >= limit or cache[cur] == 0:
            path.append(cur)
            if cur % 2 == 0:
                cur //= 2
            else:
                cur = 3 * cur + 1

        steps = cache[cur] + 1
        for i in reversed(path):
            if i < limit:
                cache[i] = steps
            steps += 1

    return str(cache.index(max(cache)))

def solve():
    start = perf_counter()
    result = compute(1_000_000)
    elapsed_time = perf_counter() - start

    return (result, "{:f}".format(elapsed_time))
