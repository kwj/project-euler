# project euler: problem 2

# f₁ = 1, f₂ = 2, f₃ = 3, f₄ = 5, f₅ = 8, f₆ = 13, f₇ = 21, f₈ = 34, f₉ = 55, ...
#
# assume that k ≥ 7
# f(k) = f(k-1) + f(k-2)
#         = 2f(k-2) + f(k-3)
#         = 2(f(k-3) + f(k-4)) + f(k-3)
#         = 3f(k-3) + 2f(k-4)
#         = 3f(k-3) + 2f(k-5) + 2f(k-6)
#         = 4f(k-3) - f(k-3) + 2f(k-5) + 2f(k-6)
#         = 4f(k-3) - (f(k-4) + f(k-5)) + 2f(k-5) + 2f(k-6)
#         = 4f(k-3) - f(k-4) + f(k-5) + 2f(k-6)
#         = 4f(k-3) - f(k-4) + (f(k-5) + f(k-6)) + f(k-6)
#         = 4f(k-3) - f(k-4) + f(k-4) + f(k-6)
#         = 4f(k-3) + f(k-6)

from collections.abc import Generator
from itertools import takewhile


def even_fib_gen() -> Generator[int, None, None]:
    a, b = 8, 2
    while True:
        a, b = 4 * a + b, a
        yield a


def compute(ulimit: int) -> str:
    return str(
        sum(i for i in takewhile(lambda n: n <= ulimit, even_fib_gen())) + (8 + 2)
    )


def solve() -> str:
    return compute(4_000_000)
