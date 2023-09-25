from math import isqrt


def is_triangular(n: int) -> bool:
    tmp = 8 * n + 1
    tmp_sqrt = isqrt(tmp)

    return tmp_sqrt * tmp_sqrt == tmp and tmp_sqrt % 2 == 1


def is_square(n: int) -> bool:
    n_sqrt = isqrt(n)

    return n_sqrt * n_sqrt == n


def is_pentagonal(n: int) -> bool:
    tmp = 24 * n + 1
    tmp_sqrt = isqrt(tmp)

    return tmp_sqrt * tmp_sqrt == tmp and tmp_sqrt % 6 == 5


def is_hexagonal(n: int) -> bool:
    tmp = 8 * n + 1
    tmp_sqrt = isqrt(tmp)

    return tmp_sqrt * tmp_sqrt == tmp and tmp_sqrt % 4 == 3
