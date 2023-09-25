# project euler: problem 36

from euler.lib.util import is_palindrome


def check_palindorme(n: int) -> bool:
    return is_palindrome(n) is True and is_palindrome(n, base=2) is True


def compute(limit: int) -> str:
    return str(sum(i for i in range(1, limit, 2) if check_palindorme(i) is True))


def solve() -> str:
    return compute(1_000_000)
