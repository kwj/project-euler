// project euler: problem 35

import { isPrime, primes } from "../lib/prime.ts";
import { range } from "../lib/util.ts";

export const compute = (limit: number): string => {
  const checkRotNum = (n: number): boolean => {
    const s = String(n) + String(n);
    const m = Math.trunc(s.length / 2);

    for (const pos of range(0, m)) {
      if (!isPrime(Number(s.slice(pos, pos + m)))) {
        return false;
      }
    }

    return true;
  };

  let acc = 0;
  for (const n of primes(limit)) {
    if (checkRotNum(n)) {
      acc += 1;
    }
  }

  return String(acc);
};

export const solve = (): string => compute(1_000_000);
