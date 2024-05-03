// project euler: problem 31

import { range } from "../lib/util.ts";

export const compute = (coins: number[], target: number): string => {
  const tbl = new Array(target + 1);
  tbl.fill(0);
  tbl[0] = 1;

  for (const c of coins) {
    for (const i of range(c, target + 1)) {
      tbl[i] += tbl[i - c];
    }
  }

  return String(tbl[target]);
};

export const solve = (): string =>
  compute([1, 2, 5, 10, 20, 50, 100, 200], 200);
