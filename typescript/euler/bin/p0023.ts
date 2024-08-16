// project euler: problem 23

import { aliquotSumTbl } from "../lib/factor.ts";
import { sum } from "../lib/math.ts";
import { range } from "../lib/util.ts";

export const compute = (limit: number): string => {
  const isSumOfTwoAbndnts = (n: number): boolean => {
    // n = x + (n-x). x is abundant number.
    // if (n-x) is abundant number, return True.
    for (const x of abndntNums) {
      if (x > Math.trunc(n / 2)) {
        break;
      }
      if (abndntFlags[n - x] === true) {
        return true;
      }
    }

    return false;
  };

  const tbl = aliquotSumTbl(limit);
  const abndntNums = range(12, limit + 1).filter((x) => x < tbl[x]);
  const abndntFlags: boolean[] = Array(limit + 1).fill(false);
  for (const i of abndntNums) {
    abndntFlags[i] = true;
  }

  return String(
    sum(range(1, limit + 1).filter((x) => isSumOfTwoAbndnts(x) === false)),
  );
};

export const solve = (): string => compute(28123);
