# project euler: problem 52

from itertools import count


def check_num(n: int) -> bool:
    def get_key(num: int) -> str:
        return ''.join(sorted(str(num)))

    key_id = get_key(n)

    return all(key_id == get_key(n * mult) for mult in range(2, 7))


def compute() -> str:
    for ndigits in count(6):
        lower = 10 ** (ndigits - 1)
        upper = (10**ndigits) // 6
        for n in range(lower, upper + 1):
            if check_num(n):
                return str(n)

    raise RuntimeError('unreachable!')


def solve() -> str:
    return compute()
