
# project euler: problem 65

'''
  e = [2; 1, 2, 1, 1, 4, 1, 1, 6, ..., 1, 1, 2k, ...]
      [a{0}; a{1}, a{2}, ...]

    i  a{i-1}  n(numerator)  d(denominator)
   ----------------------------------------
    1   2         2             1
    2   1         3             1
    3   2         8             3
    4   1        11             4
    5   1        19             7
    6   4        87            32
    7   1       106            39
    8   1       193            71
    9   6      1264           465
   10   1      1457           536
             ...
    i c(i)     n(i)          d(i)

    when i > 2:
      n(i) = n(i-1)*c(i) + n(i-2), n(2) = 3, n(1) = 2
      d(i) = d(i-1)*c(i) + d(i-2), d(2) = 1, d(1) = 1

      c(i) = | 1    (i mod 3 <> 0)
             | 2i/3 (i mod 3 = 0)

def napier_cf_gen():
    yield 2
    for i in count(2):
        yield 1 if i % 3 != 0 else (2 * i) // 3
'''

from time import perf_counter

def c(i):
    return 1 if i % 3 != 0 else (2 * i) // 3

def compute(nth):
    # n(i-1): n_i1, n(i-2): n_i2
    n_i1, n_i2 = 3, 2
    for i in range(3, nth + 1):
        n_i1, n_i2 = n_i1 * c(i) + n_i2, n_i1

    return str(sum(map(int, list(str(n_i1)))))

def solve():
    start = perf_counter()
    result = compute(100)
    elapsed_time = perf_counter() - start

    return (result, "{:f}".format(elapsed_time))
