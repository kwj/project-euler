
# project euler: problem 77

from euler.lib.prime import prime_generator
from time import perf_counter

def plst_generator():
    p_gen = prime_generator()
    plst = []
    while True:
        plst.append(next(p_gen))
        yield plst

def compute(boundary):
    plst_gen = plst_generator()
    while True:
        plst = next(plst_gen)
        tbl = [0] * (len(plst) + 1)
        tbl[0] = 1
        for i in plst:
            # Skip when i >= len(tbl)
            # This is because the prime number 'i' is larger than the sum of primes we now assume
            for j in range(i, len(tbl)):
                tbl[j] += tbl[j - i]

        # over the boundary
        if tbl[-1] > boundary:
            break

    return str(len(plst))

def solve():
    start = perf_counter()
    result = compute(5_000)
    elapsed_time = perf_counter() - start

    return (result, "{:f}".format(elapsed_time))

