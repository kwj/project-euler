// project euler: problem 54

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

  hand rank:
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

import { chunk } from "std/collections/chunk.ts";
import { unzip } from "std/collections/unzip.ts";
import { assetData } from "../lib/asset.ts";
import { cmpLst, Counter, range } from "../lib/util.ts";

function getHandRank(hand: string[]): number[] {
  function rankToNum(s: string): number {
    // deno-fmt-ignore
    const tbl = new Map<string, number>(
      [["2", 2], ["3", 3], ["4", 4], ["5", 5], ["6", 6], ["7", 7], ["8", 8],
       ["9", 9], ["T", 10], ["J", 11], ["Q", 12], ["K", 13], ["A", 14]]
    )
    return tbl.get(s) as number;
  }

  function isStraight(lst: number[]): boolean {
    return cmpLst(lst, range(lst[0], lst[0] - 5, -1)) === 0;
  }

  function getDetail(handInfo: [string, number][]): number[] {
    const [detail, _] = unzip(handInfo);
    return detail.map((x) => Number(x));
  }

  function getHand(suitLst: string[], rankLst: number[]): number[] {
    // deno-fmt-ignore
    enum Hand {
      HC = 0, OP, TP, TK, S, F, FH, FK, SF, RF
    }

    function cmpDetail(a: [string, number], b: [string, number]): number {
      const tmp = b[1] - a[1];
      if (tmp !== 0) {
        return tmp;
      }

      const a_n = Number(a[0]);
      const b_n = Number(b[0]);
      if (b_n < a_n) {
        return -1;
      } else if (b_n > a_n) {
        return 1;
      } else {
        return 0;
      }
    }

    const handInfo = Counter(rankLst).sort(cmpDetail);

    if (Counter(suitLst).length === 1) {
      if (isStraight(rankLst) === true) {
        switch (rankLst[0]) {
          case 14:
            return [Hand.RF, ...getDetail(handInfo)];
          default:
            return [Hand.SF, ...getDetail(handInfo)];
        }
      } else {
        return [Hand.F, ...getDetail(handInfo)];
      }
    } else {
      switch (handInfo.length) {
        case 5:
          if (isStraight(rankLst) === true) {
            return [Hand.S, ...getDetail(handInfo)];
          } else {
            return [Hand.HC, ...getDetail(handInfo)];
          }
        case 4:
          return [Hand.OP, ...getDetail(handInfo)];
        case 3:
          if (handInfo[0][1] === 3) {
            return [Hand.TK, ...getDetail(handInfo)];
          } else {
            return [Hand.TP, ...getDetail(handInfo)];
          }
        case 2:
          if (handInfo[0][1] === 4) {
            return [Hand.FK, ...getDetail(handInfo)];
          } else {
            return [Hand.FH, ...getDetail(handInfo)];
          }
        default:
          throw new Error("not reached");
      }
    }
  }

  const [rankLst, suitLst] = unzip(hand.map((x) => [rankToNum(x[0]), x[1]]));
  rankLst.sort().reverse();

  return getHand(suitLst, rankLst);
}

export const compute = (data: string): string => {
  function parseData(data: string): string[][][] {
    function splitLines(str: string): string[] {
      const result = str.split(/\r?\n/);
      if (result.at(-1) === "") {
        return result.slice(0, -1);
      } else {
        return result;
      }
    }

    return splitLines(data).map((x) => chunk(x.split(" "), 5));
  }

  function judge(lst: string[][]): number {
    return cmpLst(getHandRank(lst[0]), getHandRank(lst[1]));
  }

  const handLst = parseData(data);
  let p1 = 0, p2 = 0, draw = 0;

  for (const hands of handLst) {
    switch (judge(hands)) {
      case 1:
        p1 += 1;
        break;
      case -1:
        p2 += 1;
        break;
      default:
        draw += 1;
    }
  }

  return String(p1);
};

export const solve = (): string => {
  const data = new TextDecoder().decode(assetData("p054_poker.txt"));
  return compute(data);
};
