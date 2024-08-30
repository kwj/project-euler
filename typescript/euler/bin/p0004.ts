// project euler: problem 4

import { maxLst } from "../lib/math.ts";
import { isPalindrome, range } from "../lib/util.ts";

export const compute = (digits: number): string => {
  if (digits <= 0) {
    throw new Error("range error");
  }

  const nUpper = (10 ** digits) - 1;
  const nLower = 10 ** (digits - 1);
  const blkUpperLimit = 10 ** (digits * 2) - 1;
  const blkLowerLimit = digits > 1 ? 10 ** ((digits - 1) * 2) : 0;
  const blkWidth = 10 ** (digits * 2 - 2);
  const answer: number[] = [];

  for (
    const blk_lower of range(blkLowerLimit, blkUpperLimit, blkWidth)
      .toReversed()
  ) {
    const blk_upper = blk_lower + blkWidth - 1;
    for (const x of range(nLower, nUpper + 1).toReversed()) {
      if (x * x < blk_lower) {
        break;
      }
      for (
        const y of range(nLower, Math.min(Math.trunc(blk_upper / x), x) + 1)
          .toReversed()
      ) {
        const tmp = x * y;
        if (tmp < blk_lower) {
          break;
        }
        if (isPalindrome(tmp)) {
          answer.push(tmp);
        }
      }
    }
    if (answer.length !== 0) {
      break;
    }
  }

  return answer.length !== 0 ? String(maxLst(answer)) : "0";
};

export const solve = (): string => compute(3); // 3-digit
