# project euler: problem 68

from collections.abc import Sequence


def all_rings(n_gon: int) -> list[str]:
    # total: sum of each node on line
    #   minimum total: (n_gon * 2) + 1 + 2 = n_gon * 2 + 3
    #   maximum total: 1 + (n_gon * 2 - 1) + (n_gon * 2) = n_gon * 4
    result: list[str] = []
    for total in range(n_gon * 2 + 3, n_gon * 4 + 1):
        result += find_rings(n_gon, total)

    return result


def find_rings(n_gon: int, total: int) -> list[str]:
    numbers = list(range(1, n_gon * 2 + 1))
    rings: list[str] = []

    def make_str(ring: Sequence[int]) -> str:
        result = ''
        for i in range(0, n_gon * 2, 2):
            result = f'{result}{ring[i + 1]}{ring[i]}{ring[i + 2]}'

        return result

    # ring :: list[int]
    #   +-+-+--     ---+-+-+   X: first selected inner node -- ring[0]
    #   |X|Y|   ...    | |Z|   Y: first selected outer node -- ring[1]
    #   +-+-+--     ---+-+-+   Z: last selected inner node -- ring[-1]
    #    0
    #
    #     [Y]
    #       \
    #        [X]   *
    #       /   \ /
    #     ??     *
    #    / \    /
    #  ??  [Z]-*-- *
    #        \
    #         ??
    def next_states(
        state: tuple[list[int], list[int]],
    ) -> list[tuple[list[int], list[int]]]:
        ring, rest = state
        result: list[tuple[list[int], list[int]]] = []

        if len(rest) == 1:
            outer = rest[0]
            if outer > ring[1] and outer + ring[0] + ring[-1] == total:
                # a magic `n-gon` ring found
                rings.append(make_str(ring + [outer, ring[0]]))
        else:
            for outer in rest:
                if (len(ring) == 1 and outer > n_gon + 1) or (len(ring) > 1 and outer < ring[1]):
                    continue
                inner = total - outer - ring[-1]
                if outer == inner:
                    continue
                if inner not in rest:
                    continue
                tmp = rest[:]
                tmp.remove(outer)
                tmp.remove(inner)
                result.append((ring + [outer, inner], tmp))

        return result

    stack: list[tuple[list[int], list[int]]] = []
    for x in numbers:
        tmp = numbers[:]
        tmp.remove(x)
        stack.append(([x], tmp))

    while len(stack) > 0:
        state = stack.pop()
        stack += next_states(state)

    return rings


def compute(n_gon: int) -> str:
    result = sorted(all_rings(n_gon))
    if n_gon == 5:
        result = list(filter(lambda x: len(x) == 16, result))

    return result[-1]


def solve() -> str:
    return compute(5)


#   This problem can be solved with pen and paper.
#   No computer is needed.
#
#   1) maximam 16-digit string
#     number '10' must be at outer position.
#
#   2) If the condition is met when the numbers '6' to '10' are at
#      outer positions, one of them is the answer.
#
#   --> Assume that numbers '6' to '10' are at the outer positions.
#
#   3) the number '1' to '5' are on the pentagon.
#
#   4) the sum of all lines is (1+2+3+4+5)*2 + (6+7+8+9+10) = 70.
#
#   5) the sum of numbers on each lines are all 14.
#
#   6) number '1' and '3' must be on the line which contains '10'.
#
#     case #1       case #2
#         10            10
#           \             \
#            3  8,9        1  6,7,8,9
#             \ /           \ /
#              1             3
#
#   7) case #1-1:
#           10
#             \
#              3   8
#               \ /
#                1
#               /
#              5
#
#        <a>                 <b>
#           10              10
#             \               \
#              3   8           3   8
#            /  \ /          /  \ /
#           4    1   [NG]   2    1    [NG]
#          /\   /           \   /
#        [7] 2-5--7          4-5-[5]
#             \
#              8
#
#      case #1-2:
#           10
#             \
#              3   9
#               \ /
#                1
#               /
#              4
#
#        <a>                 <b>
#           10                10
#             \                 \
#              3   9             3   9
#               \ /            /  \ /
#                1   [NG]     5    1   [OK] 6,5,3;10,3,1;9,1,4;8,4,2;7,2,5
#               /            /\   /
#            5-4-[5]        6  2-4--8
#                               \
#                                7
#
#
#         <e1>
#           a0
#             \   <e2>
#             a1  a3
#            /  \ /
#          a8   a2
#          /\   /
#        a9 a6-a4-a5 <e3>
#      <e5>   \
#              a7
#              <e4>
#
#   8) case #2-1:
#           10
#             \
#              1   9
#               \ /
#                3
#               /
#              2
#        <a>                 <b>
#           10                   10
#             \                    \
#              1   9                1   9
#            /  \ /               /  \ /
#           5    3   [NG]        4    3  [NG]
#           \   /                \   /
#            4-2                  5-2
#             \                    \
#             [5]                  [5]
#
#      case #2-2:
#           10
#             \
#              1   8
#               \ /
#                3   [NG]
#               /
#             [3]
#
#      case #2-3:
#           10
#             \
#              1   7
#               \ /
#                3
#               /
#              4
#
#        <a>                 <b>
#           10                   10
#             \                    \
#              1   7                1   7
#            /  \ /               /  \ /
#           5    3   [NG]        2    3  [NG]
#           \   /                \   /
#            2-4                  5-4
#             \                    \
#             [7]                  [7]
#
#      case #2-4:
#           10
#             \
#              1   6
#               \ /
#                3
#               /
#              5
#
#        <a>                 <b>
#           10                   10
#             \                    \
#              1   6                1   6
#            /  \ /               /  \ /
#           2    3   [NG]        4    3   [OK] 6,3,5;7,5,2;8,2,4;9,4,1;10,1;3
#           \   /               /\   /
#            4-5-[5]           9  2-5-7
#                                  \
#                                   8
#
#   9) There are two patterns which satisfy the condition.
#      The answer is case #1-2 <b>.
#       case #1-2 <b>
#         6,5,3;10,3,1;9,1,4;8,4,2;7,2,5
#       case #2-4 <b>
#         6,3,5;7,5,2;8,2,4;9,4,1;10,1;3
