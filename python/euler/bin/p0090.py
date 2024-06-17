# project euler: problem 90

from itertools import combinations, combinations_with_replacement

SQUARES = [
    (0, 1),
    (0, 4),
    (0, 6),
    (1, 6),
    (2, 5),
    (3, 6),
    (4, 6),
    (8, 1),
]


def is_contain(two_dice: tuple[set[int], set[int]], sq: tuple[int, int]) -> bool:
    if sq[0] in two_dice[0] and sq[1] in two_dice[1]:
        return True
    if sq[0] in two_dice[1] and sq[1] in two_dice[0]:
        return True

    return False


def compute() -> str:
    cnt = 0
    all_dice = map(lambda x: set(x), combinations([0, 1, 2, 3, 4, 5, 6, 7, 8, 6], 6))
    for two_dice in combinations_with_replacement(all_dice, 2):
        if all(is_contain(two_dice, sq) for sq in SQUARES):
            cnt += 1

    return str(cnt)


def solve() -> str:
    return compute()
