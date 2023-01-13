
# project euler: problem 90

from euler.lib.util import flatten
from itertools import combinations, product
from time import perf_counter

def proc_69(tpl):
    if '6' in tpl or '9' in tpl:
        return tuple(sorted(set(('6', '9', *tpl))))
    else:
        return tpl

def make_numbers(d1, d2):
    return flatten([(tpl[0] + tpl[1], tpl[1] + tpl[0]) for tpl in product(d1, d2)])

def compute():
    squares = ['01', '04', '09', '16', '25', '36', '49', '64', '81']
    faces = [proc_69(tpl) for tpl in combinations(('0', '1', '2', '3', '4', '5', '6', '7', '8', '9'), 6)]

    acc = 0
    for d1, d2 in product(faces, repeat=2):
        numbers = make_numbers(d1, d2)
        if all([sq in numbers for sq in squares]) == True:
            acc += 1
    else:
        acc //= 2

    return str(acc)

def solve():
    start = perf_counter()
    result = compute()
    elapsed_time = perf_counter() - start

    return (result, "{:f}".format(elapsed_time))
