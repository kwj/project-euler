// project euler: problem 21

import { aliquotSumTbl } from "../lib/factor.ts";
import { sum } from "../lib/math.ts";
import { range } from "../lib/util.ts";

export const compute = (n: number): string => {
  const tbl = aliquotSumTbl(n - 1);
  const n_set = new Set();
  for (const i of range(2, n)) {
    if (n_set.has(i) === true) {
      continue;
    }
    const d1 = tbl[i];
    if (d1 <= i) {
      continue;
    }
    const d2 = tbl[d1];
    if (i === d2) {
      n_set.add(d1);
      n_set.add(d2);
    }
  }

  return String(sum(Array.from(n_set) as number[]));
};

export const solve = (): string => compute(10_000);
