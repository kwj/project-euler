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


# Another solution
#
# a^2 + b^2 = c^2
# a + b + c = p (perimeter)
#
# b = p - a - c
# --> b^2 = (p - a - c)^2
#
# a^2 + b^2 = c^2
# --> a^2 + (p - a - c)^2 = c^2
#     a^2 + p^2 + a^2 + c^2 - 2ap - 2ac + 2ac = c^2
#     a^2 + p^2 + a^2 - 2ap - 2cp + 2ac = 0
#     2*a^2 - 2(p - c)a + (p^2 - 2cp) = 0
#
# --> discriminant D = (-2(p - c))^2 - 4 * 2 * (p^2 - 2cp)
#                    = 4(p - c)^2 - 8(p^2 - 2cp) >= 0
# --> D/4 = (p - c)^2 + 4cp - 2p^2
#         = p^2 - 2cp + c^2 + 4cp - 2p^2
#         = c^2 + 2cp - p^2
#         = (c + p)^2 - 2p^2
#         = (c + p + p * sqrt(2)) * (c + p - p * sqrt(2)) >= 0
#
# Thus, c + p - p * sqrt(2) = c + (1 - sqrt(2))p >= 0
# --> c >= (sqrt(2) - 1)p
#
# a + b + c = p, and a + b > c
# Hence, 2c < p
#         c < p/2
#
# when p = 1000
#   c >= 0.41421356 * p = 414.21356
#   c < p/2 = 500
#   --> c ∈ [415, 500)
#
# quadratic equation:
#   2*a^2 - 2(p - c)a + (p^2 - 2cp) = 0
# discriminant D:
#   4(p - c)^2 - 8(p^2 - 2cp)
# roots of the quadratic equation:
#   (2(p - c) ± sqrt(D)) / 4
#
# ----
#from math import ceil, isqrt, sqrt
#
#def compute(p: int) -> str:
#    for c in range(ceil((sqrt(2) - 1) * p), ceil(p / 2)):
#        D = 4 * (p - c) ** 2 - 8 * (p ** 2 - 2 * c * p)
#        sqrt_D = isqrt(D)
#        if sqrt_D * sqrt_D == D:
#            _a = (2 * (p - c) - sqrt_D)
#            _b = (2 * (p - c) + sqrt_D)
#            if _a > 0 and _a % 4 == 0 and _b % 4 == 0:
#                return str((_a // 4) * (_b // 4) * c)
