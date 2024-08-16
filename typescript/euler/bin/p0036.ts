// project euler: problem 36

import { isPalindrome, range } from "../lib/util.ts";

export const compute = (limit: number): string => {
  let acc = 0;
  for (const i of range(1, limit, 2)) {
    if (isPalindrome(i, 10) && isPalindrome(i, 2)) {
      acc += i;
    }
  }

  return String(acc);
};

export const solve = (): string => compute(1_000_000);
