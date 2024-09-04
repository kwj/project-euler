// Project Euler: Problem 54

/*
  We'll need the following file to run this program.
    - https://projecteuler.net/project/resources/p054_poker.txt

  card rank:
    2, 3, 4, 5, 6, 7, 8, 9, Ten(10), Jack(11), Queen(12), King(13), Ace(14)

  category:
    0 - High Card: Highest value card.
    1 - One Pair: Two cards of the same value.
    2 - Two Pairs: Two different pairs.
    3 - Three of a Kind: Three cards of the same value.
    4 - Straight: All cards are consecutive values.
    5 - Flush: All cards of the same suit.
    6 - Full House: Three of a kind and a pair.
    7 - Four of a Kind: Four cards of the same value.
    8 - Straight Flush: All cards are consecutive values of same suit.
    9 - Royal Flush: Ten, Jack, Queen, King, Ace, in same suit.

  hand info (struct Hand):
    example:
      8H 3D JS 6S 4C -> category: 0, cards: [11, 8, 6, 4, 3]      - HC: High Card [11] @ kicker: [8; 6; 4; 3]
      9S 3C 9C 5S JS -> category: 1, cards: [9, 11, 5, 3]         - OP: One Pair [9] @ kicker : [11; 5; 3]
      5C AD 5D AC 9C -> category: 2, cards: [14, 5; 9]            - TP: Two Pair [14; 5] @ kicker : [9]
      3H 8S 7D 7H 7S -> category: 3, cards: [7, 8, 3]             - TK: Three of a Kind [7] @ kicker : [8; 3]
      7H 5D 6S 8H 9H -> category: 4, cards: [9, 8, 7, 6, 5]       - S:  Straight [9; 8; 7; 6; 5]
      2H 6H 7H QH JH -> category: 5, cards: [12, 11, 7, 6, 2]     - F:  Flush [12; 11; 7; 6; 2]
      4D 8C 8S 4S 4H -> category: 6, cards: [4, 8]                - FH: Full House [4; 8]
      3S 8H 3D 3H 3C -> category: 7, cards: [3, 8]                - FK: Four of a Kind [3] @ kicker : [8]
      8C 6C 7C 5C 9C -> category: 8, cards: [9, 8, 7, 6, 5]       - SF: Straight Flush [9; 8; 7; 6; 5]
      AH JH TH QH KH -> category: 9, cards: [14, 13, 12, 11, 10]  - RF: Royal Flush [14; 13; 12; 11; 10]
*/

use std::cmp::Ordering;
use std::collections::HashMap;

euler::run_solver!(54);

static FILE_DATA: &str = include_str!("../../assets/0054_poker.txt");

#[derive(Ord, PartialOrd, PartialEq, Eq)]
struct Hand {
    category: i64,
    cards: Vec<i64>,
}

fn solve() -> String {
    compute(FILE_DATA).to_string()
}

fn compute(data: &str) -> i64 {
    let all_games = parse_data(data);
    let mut _p1_win: i64 = 0;
    let mut _p2_win: i64 = 0;
    let mut _draw: i64 = 0;
    for cards in all_games {
        let hand_p1 = make_handinfo(&cards[0..5]);
        let hand_p2 = make_handinfo(&cards[5..10]);
        match hand_p1.cmp(&hand_p2) {
            Ordering::Greater => _p1_win += 1,
            Ordering::Less => _p2_win += 1,
            Ordering::Equal => _draw += 1,
        }
    }

    _p1_win
}

fn parse_data(data: &str) -> Vec<Vec<(i64, char)>> {
    let card_num: HashMap<char, i64> = HashMap::from([
        ('2', 2),
        ('3', 3),
        ('4', 4),
        ('5', 5),
        ('6', 6),
        ('7', 7),
        ('8', 8),
        ('9', 9),
        ('T', 10),
        ('J', 11),
        ('Q', 12),
        ('K', 13),
        ('A', 14),
    ]);
    let mut ret: Vec<Vec<(i64, char)>> = Vec::new();

    for line in data.lines() {
        let cards: Vec<(i64, char)> = line
            .split_ascii_whitespace()
            .map(|card| {
                (
                    *card_num.get(&card.chars().next().unwrap()).unwrap(),
                    card.chars().nth(1).unwrap(),
                )
            })
            .collect();
        ret.push(cards);
    }

    ret
}

fn make_handinfo(cards: &[(i64, char)]) -> Hand {
    const CAT_RF: i64 = 9;
    const CAT_SF: i64 = 8;
    const CAT_FK: i64 = 7;
    const CAT_FH: i64 = 6;
    const CAT_F: i64 = 5;
    const CAT_S: i64 = 4;
    const CAT_TK: i64 = 3;
    const CAT_TP: i64 = 2;
    const CAT_OP: i64 = 1;
    const CAT_HC: i64 = 0;

    let (nums, mut suits): (Vec<_>, Vec<_>) = cards.iter().cloned().unzip();

    // Check if a flash or not
    suits.sort();
    suits.dedup();
    let flash = suits.len() == 1;

    // get numbers and their counts
    let mut hand_tmp: Vec<(i64, usize)> = euler::countmap(nums).drain().collect();
    hand_tmp.sort_by(cmp_countmap);
    let (hand_n, hand_c): (Vec<_>, Vec<_>) = hand_tmp.into_iter().unzip();

    let mut rank = Hand {
        category: CAT_HC,
        cards: hand_n,
    };

    if flash {
        if is_straight(&rank.cards) {
            if rank.cards[0] == 14 {
                rank.category = CAT_RF;
            } else {
                rank.category = CAT_SF;
            }
        } else {
            rank.category = CAT_F;
        }
    } else {
        let num_hand_n = rank.cards.len();
        if num_hand_n == 5 {
            if is_straight(&rank.cards) {
                rank.category = CAT_S;
            } else {
                rank.category = CAT_HC;
            }
        } else if num_hand_n == 4 {
            rank.category = CAT_OP;
        } else if num_hand_n == 3 {
            if hand_c[0] == 3 {
                rank.category = CAT_TK;
            } else {
                rank.category = CAT_TP;
            }
        } else if num_hand_n == 2 {
            if hand_c[0] == 4 {
                rank.category = CAT_FK;
            } else {
                rank.category = CAT_FH;
            }
        } else {
            // Not reached
            unreachable!();
        }
    }

    rank
}

fn is_straight(lst: &[i64]) -> bool {
    if lst.len() != 5 {
        false
    } else {
        *lst == (lst[4]..(lst[4] + 5)).rev().collect::<Vec<i64>>()
    }
}

fn cmp_countmap(x: &(i64, usize), y: &(i64, usize)) -> std::cmp::Ordering {
    if x.1 != y.1 {
        (y.1).cmp(&x.1)
    } else {
        (y.0).cmp(&x.0)
    }
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0054() {
        assert_eq!(compute(super::FILE_DATA), 376);
    }
}
