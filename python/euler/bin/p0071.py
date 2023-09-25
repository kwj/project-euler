# project euler: problem 71

#  Farey sequence
#
#   2/5, 3/7
#     -> 2/5, (2+3)/(5+7), 3/7
#     -> 2/5, (2+3)/(5+7), (2+3+3)/(5+7+7), 3/7
#     -> 2/5, (2+3)/(5+7), (2+3+3)/(5+7+7), (2+3+3+3)/(5+7+7+7), 3/7
#      ...
#     -> 2/5, ..., (2+3x)/(5+7x), 3/7
#
#       5+7x <= 1_000_000


def compute(limit: int) -> str:
    return str(2 + 3 * ((limit - 5) // 7))


def solve() -> str:
    return compute(1_000_000)
