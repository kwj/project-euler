// project euler: problem 35

import { getPrimeTbl, primeTblToPrimes } from "../lib/primes.ts";
import { range } from "../lib/util.ts";

export const compute = (limit: number): string => {
  function checkRotNum(n: number): boolean {
    const s = String(n) + String(n);
    const m = Math.trunc(s.length / 2);

    for (const pos of range(0, m)) {
      if (primeTbl[Number(s.slice(pos, pos + m))] == false) {
        return false;
      }
    }

    return true;
  }

  const primeTbl = getPrimeTbl(limit);

  let acc = 0;
  for (const n of primeTblToPrimes(primeTbl)) {
    if (checkRotNum(n) === true) {
      acc += 1;
    }
  }

  return String(acc);
};

export const solve = (): string => compute(1_000_000);
