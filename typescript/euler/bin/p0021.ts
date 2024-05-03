// project euler: problem 21

import { isqrt, sum } from "../lib/math.ts";
import { range } from "../lib/util.ts";

const d = (n: number): number => {
  const lst: number[] = [];
  for (const i of range(1, isqrt(n) + 1)) {
    if (n % i !== 0) {
      continue;
    }
    if (i * i === n) {
      lst.push(i);
    } else {
      lst.push(i, Math.trunc(n / i));
    }
  }

  return sum(lst) - n;
};

export const compute = (n: number): string => {
  const n_set = new Set();
  for (const i of range(2, n)) {
    if (n_set.has(i) === true) {
      continue;
    }
    const d1 = d(i);
    if (d1 <= i) {
      continue;
    }
    const d2 = d(d1);
    if (i === d2) {
      n_set.add(d1);
      n_set.add(d2);
    }
  }

  return String(sum(Array.from(n_set) as number[]));
};

export const solve = (): string => compute(10_000);
