
# project euler: problem 53

'''
Pascal's triangle
  The entry in the n nth row and k kth column of Pascal's triangle is denoted C(n,k).

  C(n,r) = C(n-1,r-1) + C(n-1,r)
  C(n,0) = C(n,n) = 1
'''

from time import perf_counter

'''
# old version
def compute(num, boundary):
    assert num >= 1, 'range error'
    data = [1] * (num + 1)
    ans = 0 if boundary > 0 else num * 2

    for start in range(1, num):
        stop = (start + 2) // 2
        for i in range(start, stop - 1, -1):
            data[i] = data[i] + data[i - 1]
            if data[i] > boundary:
                ans += i - (start - i)
                break
        if start % 2 == 0:
            data[stop - 1] = data[stop]

    return str(ans)
'''

def compute(num, boundary):
    assert num >= 1, 'range error'
    n = x = num
    c = r = 1
    ans = 0 if boundary > 0 else num * 2

    while r <= n // 2:
        if (c := c * x // r) > boundary:
            ans += n - (r * 2) + 1
            c = c * r // n
            n -= 1
        else:
            r += 1
        x -= 1

    return str(ans)

def solve():
    start = perf_counter()
    result = compute(100, 1_000_000)
    elapsed_time = perf_counter() - start

    return (result, "{:f}".format(elapsed_time))
