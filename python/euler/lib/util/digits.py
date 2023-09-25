from functools import reduce
from math import fmod

from .misc import get_max_exp


def num_of_digits(num: int, base: int = 10) -> int:
    return get_max_exp(num, base) + 1


def is_pandigital(num: int) -> bool:
    def mk_bits(n: int) -> int:
        bits = 0
        while n > 0:
            bits |= 1 << (n % 10)
            n //= 10
        return bits

    return mk_bits(num) == (1 << num_of_digits(num)) - 1


def is_pandigital_nz(num: int) -> bool:
    def check_zero(n: int) -> bool:
        while n > 0:
            if n % 10 == 0:
                return False
            n //= 10
        return True

    return check_zero(num) and is_pandigital(num * 10)


def is_palindrome(num: int, base: int = 10) -> bool:
    x = num
    acc = 0
    while x > 0:
        acc = acc * base + (x % base)
        x //= base

    return acc == num


def digits(num: int, /, *, base: int = 10, pad: int = 1) -> list[int]:
    if num == 0:
        return [0]

    answer = [0] * max(num_of_digits(abs(num), base), pad)
    idx = 0
    while num != 0:
        answer[idx] = int(fmod(num, base))
        num = int(num / base)
        idx += 1

    return answer


def undigits(lst: list[int], /, base: int = 10) -> int:
    return reduce(lambda x, y: base * x + y, lst[::-1], 0)
