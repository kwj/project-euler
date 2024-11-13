# project euler: problem 36

from euler.lib.util import is_palindrome


def check_palindorme(n: int) -> bool:
    return is_palindrome(n) and is_palindrome(n, base=2)


def compute(limit: int) -> str:
    return str(sum(i for i in range(1, limit, 2) if check_palindorme(i)))


def solve() -> str:
    return compute(1_000_000)
