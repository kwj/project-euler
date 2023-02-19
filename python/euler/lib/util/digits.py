
from math import log, floor

def num_of_digits(num, base=10):
    if abs(num) <= 1:
        return 1
    return floor(log(abs(num), base)) + 1

def is_pandigital(num):
    def mk_bits(n):
        bits = 0
        while n > 0:
            bits |= (1 << (n % 10))
            n //= 10
        return bits

    return mk_bits(num) == (1 << num_of_digits(num)) - 1

def is_pandigital_nz(num):
    def check_zero(n):
        while n > 0:
            if n % 10 == 0:
                return False
            n //= 10
        return True

    return check_zero(num) and is_pandigital(num * 10)

def is_palindrome(num, base=10):
    x = num
    acc = 0
    while x > 0:
        acc = acc * base + (x % base)
        x //= base

    return acc == num
