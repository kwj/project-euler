// project euler: problem 36

import { isPalindrome, range } from "../lib/util.ts";

function isBothPalindrome(n: number): boolean {
  return isPalindrome(n) === true && isPalindrome(n, 2) === true;
}

export const compute = (limit: number): string => {
  let acc = 0;
  for (const i of range(1, limit, 2)) {
    if (isBothPalindrome(i) === true) {
      acc += i;
    }
  }

  return String(acc);
};

export const solve = (): string => compute(1_000_000);
