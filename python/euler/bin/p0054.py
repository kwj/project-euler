# project euler: problem 22

#   We'll need the following file to run this program.
#     - https://projecteuler.net/project/resources/p054_poker.txt
#
#   card rank:
#     2, 3, 4, 5, 6, 7, 8, 9, Ten(10), Jack(11), Queen(12), King(13), Ace(14)
#
#   hand:
#     0 - High Card: Highest value card.
#     1 - One Pair: Two cards of the same value.
#     2 - Two Pairs: Two different pairs.
#     3 - Three of a Kind: Three cards of the same value.
#     4 - Straight: All cards are consecutive values.
#     5 - Flush: All cards of the same suit.
#     6 - Full House: Three of a kind and a pair.
#     7 - Four of a Kind: Four cards of the same value.
#     8 - Straight Flush: All cards are consecutive values of same suit.
#     9 - Royal Flush: Ten, Jack, Queen, King, Ace, in same suit.
#
#   hand rank:
#     [hand, val_1, val_2, ...]  val_# : rank detail
#       example:
#         8H 3D JS 6S 4C -> [0, 11, 8, 6, 4, 3])     - High Card [0; 11] @ kicker: [8; 6; 4; 3]
#         9S 3C 9C 5S JS -> [1, 9, 11, 5, 3])        - One Pair [1; 9] @ kicker : [11; 5; 3]
#         5C AD 5D AC 9C -> [2, 14, 5; 9])           - Two Pair [2; 14; 5] @ kicker : [9]
#         3H 8S 7D 7H 7S -> [3, 7, 8, 3])            - Three of a Kind [3; 7] @ kicker : [8; 3]
#         7H 5D 6S 8H 9H -> [4, 9, 8, 7, 6, 5])      - Straight [4; 9; 8; 7; 6; 5]
#         2H 6H 7H QH JH -> [5, 12, 11, 7, 6, 2])    - Flush [5; 12; 11; 7; 6; 2]
#         4D 8C 8S 4S 4H -> [6, 4, 8])               - Full House [6; 4; 8]
#         3S 8H 3D 3H 3C -> [7, 3, 8])               - Four of a Kind [7; 3] @ kicker : [8]
#         8C 6C 7C 5C 9C -> [8, 9, 8, 7, 6, 5])      - Straight Flush [8; 9; 8; 7; 6; 5]
#         AH JH TH QH KH -> [9, 14, 13, 12, 11, 10]) - Royal Flush [9; 14; 13; 12; 11; 10]

from collections import Counter
from collections.abc import Iterator
from typing import IO


def get_handrank(hand: list[str]) -> list[int]:
    def rank_to_num(s: str) -> int:
        tbl = {
            '2': 2,
            '3': 3,
            '4': 4,
            '5': 5,
            '6': 6,
            '7': 7,
            '8': 8,
            '9': 9,
            'T': 10,
            'J': 11,
            'Q': 12,
            'K': 13,
            'A': 14,
        }
        return tbl[s]

    def is_straight(lst: list[int]) -> bool:
        return lst == list(range(lst[0], lst[0] - 5, -1))

    def get_detail(h_info: list[tuple[int, int]]) -> list[int]:
        detail, _ = zip(*h_info)
        return list(detail)

    HAND_RF = [9]
    HAND_SF = [8]
    HAND_FK = [7]
    HAND_FH = [6]
    HAND_F = [5]
    HAND_S = [4]
    HAND_TK = [3]
    HAND_TP = [2]
    HAND_OP = [1]
    HAND_HC = [0]

    rank_lst, suit_lst = zip(*hand)
    num_lst = sorted(map(lambda rank: rank_to_num(rank), rank_lst), reverse=True)
    hand_info = Counter(num_lst).most_common()

    if len(set(suit_lst)) == 1:
        # This hand contains five cards all of the same suit.
        if is_straight(num_lst) is True:
            match num_lst[0]:
                case 14:
                    handrank = HAND_RF  # Royal Flush
                case _:
                    handrank = HAND_SF  # Straight Flush
        else:
            handrank = HAND_F  # Flush
    else:
        match len(hand_info):
            case 5:
                if is_straight(num_lst) is True:
                    handrank = HAND_S  # Straight
                else:
                    handrank = HAND_HC  # High Card
            case 4:
                handrank = HAND_OP  # One Pair
            case 3:
                match hand_info[0][1]:
                    case 3:
                        handrank = HAND_TK  # Three of a Kind
                    case _:
                        handrank = HAND_TP  # Two Pair
            case 2:
                match hand_info[0][1]:
                    case 4:
                        handrank = HAND_FK  # Four of a Kind
                    case _:
                        handrank = HAND_FH  # Full House
            case _:
                assert False, 'unreachable!'

    handrank.extend(get_detail(hand_info))

    return handrank


def compute(fh: IO) -> str:
    def parse_data(fh: IO) -> Iterator[list[list[str]]]:
        return map(
            lambda lst: [lst[0:5], lst[5:]],
            [line.split(' ') for line in fh.read().splitlines()],
        )

    def determine(lst: list[list[str]]) -> int:
        p1, p2 = get_handrank(lst[0]), get_handrank(lst[1])
        if p1 > p2:
            return 1
        elif p1 < p2:
            return -1
        else:
            return 0

    hands_lst = parse_data(fh)
    p1 = p2 = draw = 0
    for hands in hands_lst:
        match determine(hands):
            case 1:
                p1 += 1
            case -1:
                p2 += 1
            case _:
                draw += 1

    return str(p1)


def solve() -> str:
    from euler.lib.resource import asset_file

    fh = asset_file('https://projecteuler.net/project/resources/p054_poker.txt')
    result = compute(fh)
    fh.close()
    return result
