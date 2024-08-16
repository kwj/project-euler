// project euler: problem 74

import { range } from "../lib/util.ts";

const factSum = (n: number): number => {
  const tbl = [1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880];
  if (n === 0) {
    return tbl[0];
  }

  let acc = 0;
  while (n > 0) {
    acc += tbl[n % 10];
    n = Math.trunc(n / 10);
  }

  return acc;
};

export const compute = (limit: number, threshold: number): string => {
  const chain_tbl: number[] = new Array(limit).fill(0);
  let cnt = 0;
  for (const n of range(1, limit)) {
    const footprints = new Set<number>();
    let steps = 0;
    let pos = n;
    while (!footprints.has(pos)) {
      if (pos < limit && chain_tbl[pos] !== 0) {
        steps += chain_tbl[pos];
        break;
      }
      footprints.add(pos);
      pos = factSum(pos);
      steps += 1;
    }

    chain_tbl[n] = steps;
    if (steps === threshold) {
      cnt += 1;
    }
  }

  return String(cnt);
};

export const solve = (): string => compute(1_000_000, 60);
