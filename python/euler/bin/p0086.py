# project euler: problem 86

#   1 <= a, b, c <= M
#
#   we can ignore rotations. there is only one case to consider.
#     1 <= a <= b <= c <= M
#      --> 2 <= a + b <= 2c
#
#       +--------F
#       |        |      * sqrt(c^2 + (a+b)^2) must be an integer
#       |--------|
#       |        | a+b >= 2
#       |        |
#       S--------+
#            c
#
#   when a+b <= c <= M
#     write a+b = x
#       (a, b) = (1, x-1), (2, x-2), ..., (x-1, 1)
#     however, because a<=b
#       num of (a,b) = floor(x/2) = floor((a+b)/2)
#
#   when a+b > c
#       num of (a,b) = floor((a+b)/2) - ((a+b-1) - c)
#
#       example: c=10, a+b=15
#         (a,b) = (1,14), ..., (5,10), (6,9), (7,8), ..., (14,1)
#                              ####################
#                 ^^^^^^^^^^^ = (a+b-1) - c
#                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ = floor((a+b)/2)
#
#       example: c=10, a+b=16
#         (a,b) = (1,15), ..., (6,10), (7,9), (8,8), ..., (15,1)
#                              ####################
#                 ^^^^^^^^^^^ = (a+b-1) - c
#                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ = floor((a+b)/2)


def compute(boundary: int) -> str:
    acc = 0
    c = 3
    while acc <= boundary:
        ab = c * 2
        while ab > 1:
            if (((c * c) + (ab * ab)) ** 0.5).is_integer():
                if ab <= c:
                    acc += ab // 2
                else:
                    acc += (ab // 2) - (ab - 1 - c)
            ab -= 1
        c += 1

    c -= 1

    return str(c)


def solve() -> str:
    return compute(1_000_000)
