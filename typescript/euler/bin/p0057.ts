// project euler: problem 57

import { range } from "../lib/util.ts";

export const compute = (n: number): string => {
  let ans = 0;
  let b = 1n, c = 1n;
  for (const _ of range(0, n)) {
    [b, c] = [2n * c + b, c + b];
    if (String(b).length > String(c).length) {
      ans += 1;
    }
  }

  return String(ans);
};

export const solve = (): string => compute(1_000);
