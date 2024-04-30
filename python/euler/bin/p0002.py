# project euler: problem 2

# f₁ = 1, f₂ = 2, f₃ = 3, f₄ = 5, f₅ = 8, f₆ = 13, f₇ = 21, f₈ = 34, f₉ = 55, ...
#
# assume that k ≥ 7
# f(k) = f(k-1) + f(k-2)
#         = 2f(k-2) + f(k-3)
#         = 2(f(k-3) + f(k-4)) + f(k-3)
#         = 3f(k-3) + 2f(k-4)
#         = 3f(k-3) + 2f(k-5) + 2f(k-6)
#         = 4f(k-3) - f(k-3) + 2f(k-5) + 2f(k-6)
#         = 4f(k-3) - (f(k-4) + f(k-5)) + 2f(k-5) + 2f(k-6)
#         = 4f(k-3) - f(k-4) + f(k-5) + 2f(k-6)
#         = 4f(k-3) - f(k-4) + (f(k-5) + f(k-6)) + f(k-6)
#         = 4f(k-3) - f(k-4) + f(k-4) + f(k-6)
#         = 4f(k-3) + f(k-6)
# -->
# E(n) = 4*E(n-1) + E(n-2)
# -->
# E(n+1) = 4*E(n) + E(n-1)
# E(n) = (E(n+1) - E(n-1)) / 4
# -->
# E(0) = 0, E(1) = 2, E(2) = 8, ...
#
# S_{n=1}^{n}(E(n)) = 1/4 * ( S_{n=1}^{n}(E(n+1)) - S_{n=1}^{n}(E(n-1)) )
#                   = 1/4 * ( S_{n=2}^{n+1}(E(n)) - S_{n=0}^{n-1}(E(n)) )
#                   = 1/4 * ( E(n+1) + E(n) - E(1) - E(0) )
#                   = 1/4 * ( E(n+1) + E(n) - 2)


def compute(ulimit: int) -> str:
    x = 2
    y = 0
    while x <= ulimit:
        x, y = 4 * x + y, x

    return str((x + y - 2) // 4)


def solve() -> str:
    return compute(4_000_000)
