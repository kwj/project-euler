
# project euler: problem 48

from time import perf_counter

def compute(exp):
    modulus = 10 ** 10

    return format(sum(pow(x, x, modulus) for x in range(1, exp + 1) if x % 10 != 0) % modulus, '010')

def solve():
    start = perf_counter()
    result = compute(1_000)
    elapsed_time = perf_counter() - start

    return (result, "{:f}".format(elapsed_time))
