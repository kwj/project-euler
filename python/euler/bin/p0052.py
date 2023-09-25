# project euler: problem 52

from itertools import count


def check_num(n: int) -> bool:
    def get_key(num: int) -> str:
        return ''.join(sorted(str(num)))

    key_id = get_key(n)
    for mult in range(2, 7):
        if key_id != get_key(n * mult):
            return False

    return True


def compute() -> str:
    for ndigits in count(6):
        lower = 10 ** (ndigits - 1)
        upper = (10**ndigits) // 6
        for n in range(lower, upper + 1):
            if check_num(n) is True:
                return str(n)

    assert False, 'unreachable!'


def solve() -> str:
    return compute()
