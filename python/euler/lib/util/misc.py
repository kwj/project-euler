
from itertools import chain, combinations

# Returns the largest exponent, e, for which base^e does not exceed num.
def get_max_exp(num, /, base):
    e = 0
    while num >= base:
        num //= base
        e += 1

    return e

def powerset(iter, /, min_len=0, max_len=0):
    if hasattr(iter, '__iter__') == True:
        lst = list(iter)
    else:
        assert False, 'type error'

    if max_len == 0 or max_len > len(lst):
        max_len = len(lst)

    return chain.from_iterable(combinations(lst, r) for r in range(min_len, max_len + 1))
