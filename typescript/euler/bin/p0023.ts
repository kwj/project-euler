// project euler: problem 23

import { sum } from "../lib/math.ts";
import { range } from "../lib/util.ts";

function getAbndntNums(limit: number): number[] {
  const sd_tbl = new Array(limit + 1);
  sd_tbl.fill(1);
  for (const i of range(2, limit + 1)) {
    for (const j of range(2 * i, limit + 1, i)) {
      sd_tbl[j] += i;
    }
  }

  return range(12, limit + 1).filter((x) => x < sd_tbl[x]);
}

export const compute = (limit: number): string => {
  function isSumOfTwoAbndnts(n: number): boolean {
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
  }

  const abndntNums = getAbndntNums(limit);
  const abndntFlags: boolean[] = new Array(limit + 1);
  abndntFlags.fill(false);
  for (const i of abndntNums) {
    abndntFlags[i] = true;
  }

  return String(
    sum(range(1, limit + 1).filter((x) => isSumOfTwoAbndnts(x) === false)),
  );
};

export const solve = (): string => compute(28123);
