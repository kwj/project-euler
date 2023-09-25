# project euler: problem 90

from itertools import combinations, product

from euler.lib.util import flatten


def proc_69(tpl: tuple[str, ...]) -> tuple[str, ...]:
    if '6' in tpl or '9' in tpl:
        return tuple(sorted(set(('6', '9', *tpl))))
    else:
        return tpl


def make_numbers(d1: tuple[str, ...], d2: tuple[str, ...]) -> list[tuple[str, str]]:
    return flatten([(tpl[0] + tpl[1], tpl[1] + tpl[0]) for tpl in product(d1, d2)])


def compute() -> str:
    squares = ['01', '04', '09', '16', '25', '36', '49', '64', '81']
    faces = [
        proc_69(tpl)
        for tpl in combinations(('0', '1', '2', '3', '4', '5', '6', '7', '8', '9'), 6)
    ]

    acc = 0
    for d1, d2 in product(faces, repeat=2):
        numbers = make_numbers(d1, d2)
        if all([sq in numbers for sq in squares]) is True:
            acc += 1
    else:
        acc //= 2

    return str(acc)


def solve() -> str:
    return compute()
