# project euler: problem 16

# number of digits of 2^1000
#   1 + floor(log10(2**1000))
#     = 1 + floor(1000 * log10(2))
#     = 1 + floor(1000 * 0.30102...)
#     = 1 + 301
#     = 302


def compute(num: int) -> str:
    return str(sum(map(int, str(pow(2, num)))))


def solve() -> str:
    return compute(1000)
