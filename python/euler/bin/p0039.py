# project euler: problem 39

# assume that a <= b < c, a + b + c = p ==> a < p/3
#
#   a^2 + b^2 = (p - a - b)^2
#   => a^2 + b^2 = p^2 -2ap - 2bp + a^2 + 2ab + b^2
#   => p^2 -2ap - 2bp + 2ab = 0
#   => 2bp - 2ab = p^2 - 2ap
#   => 2b(p - a) = p^2 - 2ap
#   => b = (p^2 - 2ap) / 2(p - a)
#
#    a  b  p  (E:even, O:odd)
#  -----------
#    E  E  E
#    E  E  O  ==> NG (contradiction, c is even because c^2 = a^2 + b^2 is even.)
#    E  O  E
#    E  O  O  ==> NG (contradiction, c is odd because c^2 = a^2 + b^2 is odd.)
#    O  E  E
#    O  E  O  ==> NG (contradiction, c is odd because c^2 = a^2 + b^2 is odd.)
#    O  O  E
#    O  O  O  ==> NG (contradiction, c is even because c^2 = a^2 + b^2 is even.)
#
#   'p' is always EVEN.


def compute(limit: int) -> str:
    def check_pair(p: int, a: int) -> bool:
        return (p * p - 2 * a * p) % (2 * (p - a)) == 0

    result = []
    for p in range(2, limit + 1, 2):
        lst = []
        for a in range(1, (p + 2) // 3):
            if check_pair(p, a):
                b = (p * p - 2 * a * p) // (2 * (p - a))
                lst.append((a, b, p - a - b))
        if len(lst) > 0:
            result.append((len(lst), p))

    result.sort(key=lambda x: x[0], reverse=True)

    return str(result[0][1])


def solve() -> str:
    return compute(1_000)
