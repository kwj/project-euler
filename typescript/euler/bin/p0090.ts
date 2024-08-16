// project euler: problem 90

import {
  combinations,
  combinationsWithReplacement,
} from "combinatorics/mod.ts";

const check_squares = (dice: number[][]): boolean => {
  // deno-fmt-ignore
  const squares = [[0, 1], [0, 4], [0, 6], [1, 6], [2, 5], [3, 6], [4, 6], [8, 1]];

  const is_contained = (x: number[]) => {
    if (dice[0].includes(x[0]) && dice[1].includes(x[1])) {
      return true;
    }
    if (dice[0].includes(x[1]) && dice[1].includes(x[0])) {
      return true;
    }

    return false;
  };

  return squares.every(is_contained);
};

export const compute = (): string => {
  let acc = 0;
  for (
    const two_dice of combinationsWithReplacement(
      combinations([0, 1, 2, 3, 4, 5, 6, 7, 8, 6], 6),
      2,
    )
  ) {
    if (check_squares(two_dice)) {
      acc += 1;
    }
  }

  return String(acc);
};

export const solve = (): string => compute();
