
# project euler: problem 53

'''
Pascal's triangle
  The entry in the n nth row and k kth column of Pascal's triangle is denoted C(n,k).

  C(n,r) = C(n-1,r-1) + C(n-1,r)
  C(n,0) = C(n,n) = 1
'''

from math import factorial
from time import perf_counter

def comb(n, r):
    return factorial(n) // factorial(r) // factorial(n - r)

def find_r(n, r, boundary):
    if (crnt := comb(n, r)) > boundary:
        # Search to the left
        while comb(n, r - 1) > boundary:
            r -= 1
        return r
    else:
        # Search to the right
        while (right := comb(n, r + 1)) <= boundary:
            if crnt > right:
                return None
            r += 1
        return r + 1

def compute(num, boundary):
    # Search for the smallest 'r' where n = 100 and nCr > boundary.
    r = 0
    while comb(num, r) <= boundary:
        r += 1

    # lst = [(n, r), ...] : 'r' is the smallest value when nCr is grater than boundary.
    lst = []
    for n in range(num, 0, -1):
        if (r := find_r(n, r, boundary)) is None:
            break
        lst.append((n, r))

    return str(sum(map(lambda tpl: tpl[0] + 1 - 2 * tpl[1], lst)))

def solve():
    start = perf_counter()
    result = compute(100, 1_000_000)
    elapsed_time = perf_counter() - start

    return (result, "{:f}".format(elapsed_time))
