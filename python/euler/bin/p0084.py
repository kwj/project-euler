# project euler: problem 84

import random

# Constants
# square positions
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

# number of squares
NUMBER_OF_SQUARES = 40

# communit chest, chance cards
NOP = 41
RAILWAY = 42
UTILITY = 43
BACK3 = 44


# Classes
class Pile:
    cards: list[int] = []
    idx = 0

    def __init__(self, lst: list[int]) -> None:
        assert len(lst) > 0, 'empty list'
        self.cards = lst
        self.idx = 0

    def shuffle(self) -> None:
        random.shuffle(self.cards)
        self.idx = 0


class CommunityChest(Pile):
    def __init__(self, lst: list[int]) -> None:
        super().__init__(lst)

    def get(self, sq: int) -> int:
        tmp = self.cards[self.idx]
        self.idx = (self.idx + 1) % len(self.cards)

        if tmp == NOP:
            return sq
        else:
            return tmp


class ChanceCard(Pile):
    def __init__(self, lst: list[int]) -> None:
        super().__init__(lst)

    def _nextR(self, sq: int) -> int:
        if sq == CH1:
            return R2
        elif sq == CH2:
            return R3
        elif sq == CH3:
            return R1
        else:
            assert False, 'unreachable'

    def _nextU(self, sq: int) -> int:
        if sq == CH1:
            return U1
        elif sq == CH2:
            return U2
        elif sq == CH3:
            return U1
        else:
            assert False, 'unreachable'

    def get(self, sq: int) -> int:
        tmp = self.cards[self.idx]
        self.idx = (self.idx + 1) % len(self.cards)

        if tmp == NOP:
            return sq
        elif (
            tmp == GO or tmp == JAIL or tmp == C1 or tmp == E3 or tmp == H2 or tmp == R1
        ):
            return tmp
        elif tmp == RAILWAY:
            return self._nextR(sq)
        elif tmp == UTILITY:
            return self._nextU(sq)
        elif tmp == BACK3:
            return (sq + 37) % NUMBER_OF_SQUARES
        else:
            assert False, 'unreachable'


class Dice:
    side = 4
    double_cnt = 0

    def __init__(self, side: int) -> None:
        self.side = side
        self.double_cnt = 0

    def roll(self, sq: int) -> int:
        d1 = random.randint(1, self.side)
        d2 = random.randint(1, self.side)
        if d1 == d2:
            if self.double_cnt == 2:
                self.double_cnt = 0
                return JAIL
            else:
                self.double_cnt += 1
        else:
            self.double_cnt = 0

        return (sq + d1 + d2) % NUMBER_OF_SQUARES


def compute(faces: int, n_attempts: int) -> str:
    c_chest = CommunityChest(
        [GO, JAIL, NOP, NOP, NOP, NOP, NOP, NOP, NOP, NOP, NOP, NOP, NOP, NOP, NOP, NOP]
    )
    c_chest.shuffle()
    c_card = ChanceCard(
        [
            GO,
            JAIL,
            C1,
            E3,
            H2,
            R1,
            RAILWAY,
            RAILWAY,
            UTILITY,
            BACK3,
            NOP,
            NOP,
            NOP,
            NOP,
            NOP,
            NOP,
        ]
    )
    c_card.shuffle()
    dice = Dice(faces)

    counter_tbl = [0] * NUMBER_OF_SQUARES
    current_sq = GO
    for _ in range(n_attempts):
        next_sq = dice.roll(current_sq)
        if next_sq == G2J:
            next_sq = JAIL
        elif next_sq == CC1 or next_sq == CC2 or next_sq == CC3:
            next_sq = c_chest.get(next_sq)
        elif next_sq == CH1 or next_sq == CH2 or next_sq == CH3:
            next_sq = c_card.get(next_sq)

        counter_tbl[next_sq] += 1
        current_sq = next_sq

    result = sorted(
        ((idx, cnt) for idx, cnt in enumerate(counter_tbl)),
        key=lambda tpl: tpl[1],
        reverse=True,
    )
    return '{:02}{:02}{:02}'.format(result[0][0], result[1][0], result[2][0])


def solve() -> str:
    return compute(4, 1_000_000)
