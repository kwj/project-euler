// project euler: problem 4

import { max } from "../lib/math.ts";
import { isPalindrome, range } from "../lib/util.ts";

export const compute = (digits: number): string => {
  if (digits <= 0) {
    throw new Error("range error");
  }

  const nUpper = (10 ** digits) - 1;
  const nLower = (10 ** (digits - 1)) - 1;
  const blkUpperLimit = 10 ** (digits * 2);
  const blkLowerLimit = digits > 1 ? 10 ** ((digits - 1) * 2) : 0;
  const blkWidth = 10 ** (digits * 2 - 2);
  const answer = [];

  for (const blk_upper of range(blkUpperLimit, blkLowerLimit, -blkWidth)) {
    const blk_lower = blk_upper - blkWidth;
    for (const x of range(nUpper, nLower, -1)) {
      if (x * x < blk_lower) {
        break;
      }
      for (
        const y of range(Math.min(Math.trunc(blk_upper / x), x), nLower, -1)
      ) {
        const tmp = x * y;
        if (tmp < blk_lower) {
          break;
        }
        if (isPalindrome(tmp) === true) {
          answer.push(tmp);
        }
      }
    }
    if (answer.length !== 0) {
      break;
    }
  }

  return answer.length !== 0 ? String(max(answer)) : "0";
};

export const solve = (): string => compute(3); // 3-digit
