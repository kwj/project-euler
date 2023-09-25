# project euler: problem 84

import random
from collections import Counter
from collections.abc import Callable


# square positions
class SQ:
    GO = 0
    A1 = 1
    CC1 = 2
    A2 = 3
    T1 = 4
    R1 = 5
    B1 = 6
    CH1 = 7
    B2 = 8
    B3 = 9
    JAIL = 10
    C1 = 11
    U1 = 12
    C2 = 13
    C3 = 14
    R2 = 15
    D1 = 16
    CC2 = 17
    D2 = 18
    D3 = 19
    FP = 20
    E1 = 21
    CH2 = 22
    E2 = 23
    E3 = 24
    R3 = 25
    F1 = 26
    F2 = 27
    U2 = 28
    F3 = 29
    G2J = 30
    G1 = 31
    G2 = 32
    CC3 = 33
    G3 = 34
    R4 = 35
    CH3 = 36
    H1 = 37
    T2 = 38
    H2 = 39


# Community Chest
def community_chest(sq: int) -> int:
    match random.randrange(16):
        case 0:
            return SQ.GO
        case 1:
            return SQ.JAIL
        case _:
            return sq  # nop


# Chance card
def chance_card(sq: int) -> int:
    # Railway Company
    def next_r(sq: int) -> int:
        match sq:
            case SQ.CH1:
                return SQ.R2
            case SQ.CH2:
                return SQ.R3
            case SQ.CH3:
                return SQ.R1
            case _:
                assert False, 'bad square'

    # Utility Company
    def next_u(sq: int) -> int:
        match sq:
            case SQ.CH1 | SQ.CH3:
                return SQ.U1
            case SQ.CH2:
                return SQ.U2
            case _:
                assert False, 'bad square'

    match random.randrange(16):
        case 0:
            return SQ.GO
        case 1:
            return SQ.JAIL
        case 2:
            return SQ.C1
        case 3:
            return SQ.E3
        case 4:
            return SQ.H2
        case 5:
            return SQ.R1
        case 6:
            return next_r(sq)
        case 7:
            return next_r(sq)
        case 8:
            return next_u(sq)
        case 9:
            return (sq + 37) % 40  # go back three squares
        case _:
            return sq  # nop


def monte_carlo(dice: Callable[..., int], loop_cnt: int) -> str:
    random.seed()
    counter = [0] * 40
    sq = SQ.GO
    double = 0
    for _ in range(loop_cnt):
        d1, d2 = dice(), dice()
        double = 0 if d1 != d2 else double + 1
        if double >= 3:
            sq = SQ.JAIL
            double = 0
        else:
            match (sq := (sq + d1 + d2) % 40):
                case SQ.G2J:
                    sq = SQ.JAIL
                case SQ.CC1 | SQ.CC2 | SQ.CC3:
                    sq = community_chest(sq)
                case SQ.CH1 | SQ.CH2 | SQ.CH3:
                    sq = chance_card(sq)

        counter[sq] += 1

    result, _ = zip(*sorted(enumerate(counter), key=lambda tpl: tpl[1], reverse=True))

    return '{:02}{:02}{:02}'.format(*result[:3])


def compute(faces: int, n_attempts: int, loop_cnt: int) -> str:
    def make_dice(faces: int) -> Callable[..., int]:
        def throw_die() -> int:
            return random.randint(1, faces)

        return throw_die

    dice = make_dice(faces)
    result = []
    for _ in range(n_attempts):
        result.append(monte_carlo(dice, loop_cnt))

    return Counter(result).most_common(1)[0][0]


def solve() -> str:
    return compute(4, 100, 10_000)
