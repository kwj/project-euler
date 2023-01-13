
# project euler: problem 28

# 21 22 23 24 25
# 20  7  8  9 10
# 19  6  1  2 11
# 18  5  4  3 12
# 17 16 15 14 13
#        |  |  |  |
#     (n=0, 1, 2, 3, ...)
#
# the upper right number is:
#   1    [n=0]
#   (2n+1)**2    [n=>1]
#
# so, the sum of numbers in the four corners is:
#   (2n+1)**2 + ((2n+1)**2 - 2n) + ((2n+1)**2 - 4n) + ((2n+1)**2 - 6n)
#     = 16n**2 + 4n + 4   [n>=1]
#
# Answer: 1 + sum_{n=1}^{(1001-1)/2} (16n**2 + 4n + 4)

from time import perf_counter

def compute(side_len):
    result = 1
    for n in range(1, ((side_len - 1) // 2) + 1):
        result += 16 * n * n + 4 * n + 4

    return str(result)

def solve():
    start = perf_counter()
    result = compute(1_001)
    elapsed_time = perf_counter() - start

    return (result, "{:f}".format(elapsed_time))
