// project euler: problem 55

import { isPalindrome, range } from "../lib/util.ts";

export const compute = (limit: number): string => {
  let acc = 0;

  loop_i:
  for (let i of range(0, limit)) {
    for (const _ of range(0, 50)) {
      i += Number(String(i).split("").reverse().join(""));
      if (isPalindrome(i) === true) {
        continue loop_i;
      }
    }
    acc += 1;
  }

  return String(acc);
};

export const solve = (): string => compute(10_000);
