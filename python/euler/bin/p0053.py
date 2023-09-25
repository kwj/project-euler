# project euler: problem 53

# Pascal's triangle
#   The entry in the n nth row and k kth column of Pascal's triangle is denoted C(n,k).

#   C(n,r) = C(n-1,r-1) + C(n-1,r)
#   C(n,0) = C(n,n) = 1


def compute(num: int, boundary: int) -> str:
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


def solve() -> str:
    return compute(100, 1_000_000)
