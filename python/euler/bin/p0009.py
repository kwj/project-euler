# project euler: problem 9

# a^2 + b^2 = c^2
# a + b + c = p = 1000 (perimeter)
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
# --> c >= (sqrt(2) - 1)p = 0.414213562373 * p
#                         = 414.213562373
# c is an integer, therefore, c >= 415
#
# a + b + c = p, and a + b > c
# Hence, 2c < p = 1000
#         c < 500
#
# Therefore, c ∈ [415, 500)

from math import isqrt


def compute(p: int) -> str:
    for c in range(415, 500):
        # quadratic equation:
        #    2*a^2 - 2(p - c)a + (p^2 - 2cp) = 0
        # discriminant D:
        #    4(p - c)^2 - 8(p^2 - 2cp)
        # roots of the quadratic equation:
        #    (2(p - c) ± sqrt(D)) / 4

        # d <- D/4, so sqrt(D) = sqrt(4d) = 2 * sqrt(d)
        d = c * c + 2 * c * p - p * p
        sqrt_d = isqrt(d)
        if sqrt_d * sqrt_d == d:
            # a < b, _a = 2a, _b = 2b
            _a = p - c - sqrt_d
            _b = p - c + sqrt_d
            if _a > 0 and _a & 0b1 == 0 and _b & 0b1 == 0:
                return str((_a // 2) * (_b // 2) * c)

    return str(0)  # not found (NOT REACHED when perimeter = 1000)


def solve() -> str:
    return compute(1000)
