// project euler: problem 84

/*
  Monte Carlo method
*/

import { unzip, zip } from "@std/collections";
import { Counter, range } from "../lib/util.ts";

// deno-fmt-ignore
enum Square {
  GO = 0,    A1 = 1,   CC1 = 2,  A2 = 3,   T1 = 4,
  R1 = 5,    B1 = 6,   CH1 = 7,  B2 = 8,   B3 = 9,
  JAIL = 10, C1 = 11,  U1 = 12,  C2 = 13,  C3 = 14,
  R2 = 15,   D1 = 16,  CC2 = 17, D2 = 18,  D3 = 19,
  FP = 20,   E1 = 21,  CH2 = 22, E2 = 23,  E3 = 24,
  R3 = 25,   F1 = 26,  F2 = 27,  U2 = 28,  F3 = 29,
  G2J = 30,  G1 = 31,  G2 = 32,  CC3 = 33, G3 = 34,
  R4 = 35,   CH3 = 36, H1 = 37,  T2 = 38,  H2 = 39,
}

const communityChest = (sq: number): number => {
  switch (Math.trunc(Math.random() * 16)) {
    case 0:
      return Square.GO;
    case 1:
      return Square.JAIL;
    default:
      return sq;
  }
};

const chanceCard = (sq: number): number => {
  const nextR = (sq: number): number => {
    switch (sq) {
      case Square.CH1:
        return Square.R2;
      case Square.CH2:
        return Square.R3;
      case Square.CH3:
        return Square.R1;
      default:
        throw new RangeError(`invalid square: ${sq}`);
    }
  };

  const nextU = (sq: number): number => {
    switch (sq) {
      case Square.CH1:
        return Square.U1;
      case Square.CH2:
        return Square.U2;
      case Square.CH3:
        return Square.U1;
      default:
        throw new RangeError(`invalid square: ${sq}`);
    }
  };

  switch (Math.trunc(Math.random() * 16)) {
    case 0:
      return Square.GO;
    case 1:
      return Square.JAIL;
    case 2:
      return Square.C1;
    case 3:
      return Square.E3;
    case 4:
      return Square.H2;
    case 5:
      return Square.R1;
    case 6:
    case 7:
      return nextR(sq);
    case 8:
      return nextU(sq);
    case 9:
      // go back three squares
      return (sq + 37) % 40;
    default:
      // nop
      return sq;
  }
};

const monteCarlo = (dice: () => number, loopCnt: number): string => {
  const counter: number[] = new Array(40).fill(0);
  let sq = Square.GO;
  let double = 0;

  for (const _ of range(0, loopCnt)) {
    const d1 = dice();
    const d2 = dice();
    double = (d1 !== d2) ? 0 : double + 1;
    if (double >= 3) {
      sq = Square.JAIL;
      double = 0;
    } else {
      sq = (sq + d1 + d2) % 40;
      switch (sq) {
        case Square.G2J:
          sq = Square.JAIL;
          break;
        case Square.CC1:
        case Square.CC2:
        case Square.CC3:
          sq = communityChest(sq);
          break;
        case Square.CH1:
        case Square.CH2:
        case Square.CH3:
          sq = chanceCard(sq);
          break;
      }
    }
    counter[sq] += 1;
  }

  const [result, _] = unzip(
    zip(range(0, counter.length), counter).sort((a, b) => b[1] - a[1]),
  );

  return result.slice(0, 3).map((x) => String(x).padStart(2, "0")).join("");
};

export const compute = (
  faces: number,
  nAttempts: number,
  loopCnt: number,
): string => {
  const makeDice = (faces: number): () => number => {
    return () => Math.trunc(Math.random() * faces + 1);
  };

  const dice = makeDice(faces);
  const result: string[] = [];
  for (const _ of range(0, nAttempts)) {
    result.push(monteCarlo(dice, loopCnt));
  }

  return Counter(result).sort((a, b) => b[1] - a[1])[0][0];
};

export const solve = (): string => compute(4, 1000, 10_000);
