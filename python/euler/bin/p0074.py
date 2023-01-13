
# project euler: problem 74

# naive version

from time import perf_counter

def fact_sum(n):
    tbl = [1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880]
    if n == 0:
        return tbl[0]
    acc = 0
    while n > 0:
        acc += tbl[n % 10]
        n //= 10

    return acc

def compute(limit, threshold):
    chain_tbl = [0] * limit
    cnt = 0
    for n in range(1, limit):
        steps = 0
        footprints = set()
        pos = n
        while pos not in footprints:
            if pos < limit and chain_tbl[pos] != 0:
                steps += chain_tbl[pos]
                break
            footprints.add(pos)
            pos = fact_sum(pos)
            steps += 1

        chain_tbl[n] = steps
        if steps == threshold:
            cnt += 1

    return str(cnt)

def solve():
    start = perf_counter()
    result = compute(1_000_000, 60)
    elapsed_time = perf_counter() - start

    return (result, "{:f}".format(elapsed_time))
