# project euler: problem 9

# a = k(m^2 - n^2), b = k * 2mn, c = k(m^2 + n^2)  [m>n>0, gcd(m,n)=1, m+n is odd]
#
# abc = k^3 * (m^4 - n^4) * 2mn
# a + b + c = k * 2m(m+n) = 1000
#
#  -> 'k' and 'm' are divisors to 500 (= 1000/2).
#     'm+n' is a divisor to 500/m.
#     m(m+n) <= 500 --> m <= isqrt(500), m+n <= 500/m

from math import gcd, isqrt


def compute(perim: int) -> str:
    for m in range(2, isqrt(perim // 2) + 1):
        if (perim // 2) % m != 0:
            continue

        x = m + 1 + (m % 2)  # x = m + n, x is odd number
        while x < 2 * m and x <= (perim // 2) // m:
            if gcd(m, x) == 1 and (perim // 2) // m % x == 0:
                k = (perim // 2) // m // x
                n = x - m
                return str(pow(k, 3) * (pow(m, 4) - pow(n, 4)) * 2 * m * n)
            x += 2

    return str(0)  # not found (NOT REACHED when perimeter = 1000)


def solve() -> str:
    return compute(1000)
