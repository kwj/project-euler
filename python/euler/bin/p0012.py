# project euler: problem 12

# triangle number's formula is n(n + 1)/2 and 'n' and 'n + 1' are coprime.
# Therefore, ...
#   - 'n/2' and 'n+1' are coprime (when 'n' is even)
#   - 'n' and '(n+1)/2' are coprime (when 'n' is odd)
#
# assume that f(n) returns number of divisors of 'n'.
# f(a*b) = f(a) * f(b) when 'a' and 'b' are coprime.

from euler.lib.util import num_of_divisors


def compute(limit: int) -> str:
    n = 1
    while True:
        if num_of_divisors(n) * num_of_divisors((n + 1) // 2) > limit:
            break
        n += 1
        if num_of_divisors(n // 2) * num_of_divisors(n + 1) > limit:
            break
        n += 1

    return str(n * (n + 1) // 2)


def solve() -> str:
    return compute(500)
