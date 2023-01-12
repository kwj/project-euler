
# project euler: problem 16

# number of digits of 2^1000
#   1 + floor(log10(2**1000)) = 1 + floor(1000 * log10(2)) = 1 + floor(1000 * 0.30102...) = 1 + 301 = 302

from time import perf_counter

def compute(num):
    return str(sum(map(int, str(pow(2, num)))))

def solve():
    start = perf_counter()
    result = compute(1000)
    elapsed_time = perf_counter() - start

    return (result, "{:f}".format(elapsed_time))
