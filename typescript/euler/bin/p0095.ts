// project euler: problem 95

import { aliquotSumTbl } from "../lib/factor.ts";
import { range } from "../lib/util.ts";

export const compute = (limit: number): string => {
  const checkLoop = (pos: number): [number, number] => {
    const stop = pos;
    let minValue = pos;
    let cnt = 1;
    while ((pos = sdTbl[pos]) !== stop) {
      cnt += 1;
      if (minValue > pos) {
        minValue = pos;
      }
    }

    return [cnt, minValue];
  };

  // lookup table: sum of divisors
  const sdTbl = aliquotSumTbl(limit);

  // chain table
  const chainTbl: number[] = Array(limit + 1).fill(0);
  chainTbl[1] = 1;

  let answer = 0;
  let maxChains = 0;

  for (const idx of range(2, limit + 1)) {
    // already checked
    if (chainTbl[idx] !== 0) {
      continue;
    }
    chainTbl[idx] = idx;

    let pos = idx;
    while (true) {
      const next_idx = sdTbl[pos];

      // outside the range
      if (next_idx > limit) {
        break;
      }

      // found a loop
      if (chainTbl[next_idx] === idx) {
        // perfect number
        if (next_idx === pos) {
          break;
        }
        // amicable pair
        if (sdTbl[next_idx] === pos) {
          break;
        }

        // amicable chain
        const [chains, minValue] = checkLoop(next_idx);
        if (chains > maxChains) {
          maxChains = chains;
          answer = minValue;
        }
      }

      // arrive at a known chain
      if (chainTbl[next_idx] !== 0) {
        break;
      }

      chainTbl[next_idx] = idx;
      pos = next_idx;
    }
  }

  return String(answer);
};

export const solve = (): string => compute(1_000_000);
