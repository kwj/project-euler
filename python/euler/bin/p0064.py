
# project euler: problem 64

'''
            sqrt(N) + b0        1              1
  sqrt(N) = ------------ = a0 + --,  x1 = a1 + --, ...
                 c0             x1             x2

                  c0             c0(sqrt(N) - (b0 - a0c0))
    x1 = --------------------- = -------------------------
         sqrt(N) + (b0 - a0c0)       N - (b0 - a0c0)^2

         sqrt(N) + (a0c0 - b0)   sqrt(N) + b1         1
       = --------------------- = ------------- = a1 + --
           N - (a0c0 - b0)^2          c1              x2
           -----------------
                  c0
   -->
     a{n} = floor( (sqrt(N)+b{n}) / c{n} )
     b{n+1} = a{n}*c{n} - b{n}
     c{n+1} = (N - b{n+1}^2) / c{n}

     b{0} = 0, c{0} = 1, a{0} = sqrt(N)
'''

from math import isqrt
from time import perf_counter

def get_cont_fraction(n):
    isqrt_n = isqrt(n)
    if isqrt_n * isqrt_n == n:
        return [isqrt_n, []]

    stop_condition = 2 * isqrt_n
    b = 0; c = 1; a = (isqrt_n + b) // c
    rep = []
    while True:
        b = a * c - b
        c = (n - b * b) // c
        a = (isqrt_n + b) // c
        rep.append(a)
        if a == stop_condition:    # otherwise, c == 1
            return [isqrt_n, rep]

def compute(limit):
    cnt = 0
    for n in range(1, limit + 1):
        if len(get_cont_fraction(n)[1]) % 2 == 1:
            cnt += 1

    return str(cnt)

def solve():
    start = perf_counter()
    result = compute(10_000)
    elapsed_time = perf_counter() - start

    return (result, "{:f}".format(elapsed_time))
