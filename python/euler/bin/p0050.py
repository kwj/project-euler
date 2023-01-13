
# project euler: problem 50

from euler.lib.prime import is_prime, prime_generator
from itertools import dropwhile
from time import perf_counter

def cumsum_generator():
    acc = 0
    p_gen = prime_generator()
    while True:
        acc += next(p_gen)
        yield acc

# Returns a cumulative sum list of prime numbers.
#   [0, p1, p1+p2, p1+p2+p3, ..., p1+...+p{n-1}, p1+...+p{n-1}+p{n}]
#     where sum(p1..p{n-1}) < limit and sum(p1..p{n}) >= limit
def init_cumsum_lst(cs_gen, limit):
    lst = [0]
    while lst[-1] < limit:
        lst.append(next(cs_gen))

    return lst

def compute(limit):
    cs_gen = cumsum_generator()
    cs_lst = init_cumsum_lst(cs_gen, limit)

    ans = 0
    i = 0
    width = 1
    while cs_lst[i + width] - cs_lst[i] < limit:
        begin = cs_lst[i]
        lst = list(dropwhile(lambda p: p - begin >= limit or is_prime(p - begin) == False, cs_lst[i + width:][::-1]))
        if len(lst) > 0:
            width += len(lst)
            ans = lst[0] - begin
        cs_lst.append(next(cs_gen))
        i += 1

    return str(ans)

def solve():
    start = perf_counter()
    result = compute(1_000_000)
    elapsed_time = perf_counter() - start

    return (result, "{:f}".format(elapsed_time))
