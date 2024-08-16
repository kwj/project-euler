// project euler: problem 52

import { range } from "../lib/util.ts";

const checkNum = (n: number): boolean => {
  const getKey = (n: number): string => String(n).split("").sort().join("");

  const keyId = getKey(n);
  for (const mult of range(2, 7)) {
    if (keyId !== getKey(n * mult)) {
      return false;
    }
  }

  return true;
};

export const compute = (): string => {
  let ndigits = 6;
  while (true) {
    const lower = 10 ** (ndigits - 1);
    const upper = Math.trunc((10 ** ndigits) / 6);
    for (const n of range(lower, upper + 1)) {
      if (checkNum(n)) {
        return String(n);
      }
    }
    ndigits += 1;
  }
};

export const solve = (): string => compute();
