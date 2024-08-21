// Project Euler: Problem 54

/*
  We'll need the following file to run this program.
    - https://projecteuler.net/project/resources/p054_poker.txt

  card rank:
    2, 3, 4, 5, 6, 7, 8, 9, Ten(10), Jack(11), Queen(12), King(13), Ace(14)

  hand:
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

  hand info:
    [hand, val_1, val_2, ...]  val_# : rank detail
      example:
        8H 3D JS 6S 4C -> [0, 11, 8, 6, 4, 3]      - HC: High Card [0; 11] @ kicker: [8; 6; 4; 3]
        9S 3C 9C 5S JS -> [1, 9, 11, 5, 3]         - OP: One Pair [1; 9] @ kicker : [11; 5; 3]
        5C AD 5D AC 9C -> [2, 14, 5; 9]            - TP: Two Pair [2; 14; 5] @ kicker : [9]
        3H 8S 7D 7H 7S -> [3, 7, 8, 3]             - TK: Three of a Kind [3; 7] @ kicker : [8; 3]
        7H 5D 6S 8H 9H -> [4, 9, 8, 7, 6, 5]       - S:  Straight [4; 9; 8; 7; 6; 5]
        2H 6H 7H QH JH -> [5, 12, 11, 7, 6, 2]     - F:  Flush [5; 12; 11; 7; 6; 2]
        4D 8C 8S 4S 4H -> [6, 4, 8]                - FH: Full House [6; 4; 8]
        3S 8H 3D 3H 3C -> [7, 3, 8]                - FK: Four of a Kind [7; 3] @ kicker : [8]
        8C 6C 7C 5C 9C -> [8, 9, 8, 7, 6, 5]       - SF: Straight Flush [8; 9; 8; 7; 6; 5]
        AH JH TH QH KH -> [9, 14, 13, 12, 11, 10]  - RF: Royal Flush [9; 14; 13; 12; 11; 10]
*/

use std::cmp::Ordering;
use std::collections::HashMap;

euler::run_solver!(54);

static FILE_DATA: &str = include_str!("../../assets/0054_poker.txt");

fn solve() -> String {
    compute(FILE_DATA).to_string()
}

fn compute(data: &str) -> i64 {
    let all_hands = parse_data(data);
    let mut _p1: i64 = 0;
    let mut _p2: i64 = 0;
    let mut _draw: i64 = 0;
    for hands in all_hands {
        let result = judge(
            make_handinfo(hands[0..5].to_vec()),
            make_handinfo(hands[5..10].to_vec()),
        );
        if result == 1 {
            _p1 += 1;
        } else if result == -1 {
            _p2 += 1;
        } else {
            _draw += 1;
        };
    }
    _p1
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

fn judge(p1: Vec<i64>, p2: Vec<i64>) -> i64 {
    match p1.cmp(&p2) {
        Ordering::Greater => 1,
        Ordering::Less => -1,
        Ordering::Equal => 0,
    }
}

fn make_handinfo(cards: Vec<(i64, char)>) -> Vec<i64> {
    const HAND_RF: i64 = 9;
    const HAND_SF: i64 = 8;
    const HAND_FK: i64 = 7;
    const HAND_FH: i64 = 6;
    const HAND_F: i64 = 5;
    const HAND_S: i64 = 4;
    const HAND_TK: i64 = 3;
    const HAND_TP: i64 = 2;
    const HAND_OP: i64 = 1;
    const HAND_HC: i64 = 0;

    let (nums, mut suits): (Vec<_>, Vec<_>) = cards.into_iter().unzip();

    // Check if a flash or not
    suits.sort();
    suits.dedup();
    let flash = suits.len() == 1;

    // get numbers and their counts
    let mut hand_tmp: Vec<(i64, usize)> = euler::countmap(nums).drain().collect();
    hand_tmp.sort_by(cmp_countmap);
    let (mut hand_n, hand_c): (Vec<_>, Vec<_>) = hand_tmp.into_iter().unzip();

    if flash {
        if is_straight(&hand_n) {
            if hand_n[0] == 14 {
                hand_n.insert(0, HAND_RF);
            } else {
                hand_n.insert(0, HAND_SF);
            }
        } else {
            hand_n.insert(0, HAND_F);
        }
    } else {
        let num_hand_n = hand_n.len();
        if num_hand_n == 5 {
            if is_straight(&hand_n) {
                hand_n.insert(0, HAND_S);
            } else {
                hand_n.insert(0, HAND_HC);
            }
        } else if num_hand_n == 4 {
            hand_n.insert(0, HAND_OP);
        } else if num_hand_n == 3 {
            if hand_c[0] == 3 {
                hand_n.insert(0, HAND_TK);
            } else {
                hand_n.insert(0, HAND_TP);
            }
        } else if num_hand_n == 2 {
            if hand_c[0] == 4 {
                hand_n.insert(0, HAND_FK);
            } else {
                hand_n.insert(0, HAND_FH);
            }
        } else {
            // Not reached on this problem
            unreachable!();
        }
    }
    hand_n
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
