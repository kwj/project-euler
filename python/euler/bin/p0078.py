
# project euler: problem 78

'''
  p(5) = 7
  p(10) = 42
  p(50) = 204226
  p(100) = 190569292
  p(200) = 3972999029388
  p(500) = 2300165032574323995027
  p(1000) = 24061467864032622473692149727991
    ...

  I needed to find another way instead of dynamic programming.
  Unfortunately, I gave up trying to solve it on my own at last.

  I saw following pages.

    https://en.wikipedia.org/wiki/Partition_(number_theory)
    https://en.wikipedia.org/wiki/Partition_function_(number_theory)
    https://en.wikipedia.org/wiki/Pentagonal_number_theorem

    p(n) = Sigma{k ∈ Z/{0}} (-1)^(k+1) * p(n - k(3k-1)/2)
         = p(n-1) + p(n-2) - p(n-5) - p(n-7) + p(n-12) + p(n-15) - p(n-22) - ...

      [p(0) = 1, p(k) = 0 when k < 0]

  I consider only value of 'mod 1_000_000' because the problem is divisible by one million or not.
'''

from time import perf_counter

def gen_gpnum_diff():
    # Generalized pentagonal numbers
    #        0   1   2   5   7   12   15   22   26   35   40   51   57   70   77   92   100   117  ...
    # diff:    1   1   3   2   5    3    7    4    9    5    11   6    13   7    15    8    17
    # g/s      g   s   g   s   g    s    g    s    g    s     g   s     g   s     g    s     g
    #   [g: gap, s: step]
    gap = 1
    step = 1
    while True:
        yield gap
        yield step
        gap += 2
        step += 1

def compute(denom):
    p_tbl = dict()
    p_tbl[0] = 1

    n = 0
    while p_tbl[n] != 0:
        n += 1
        param = n
        diff_gen = gen_gpnum_diff()
        sign = 1
        acc = 0
        while param >= 0:
            param -= next(diff_gen)
            acc += sign * p_tbl.get(param, 0)
            param -= next(diff_gen)
            acc += sign * p_tbl.get(param, 0)
            sign = -sign

        p_tbl[n] = acc % denom

    return str(n)

def solve():
    start = perf_counter()
    result = compute(1_000_000)
    elapsed_time = perf_counter() - start

    return (result, "{:f}".format(elapsed_time))

