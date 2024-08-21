// project euler: problem 80

/*
  when n < 100,

    i <= 10^99 * sqrt(n) < i + 1
   -->
    i^2 <= 10^198 * n < (i + 1)^2

  'i' is the 100-digit number we want.
*/

import { isqrt, sum } from "../lib/math.ts";
import { range } from "../lib/util.ts";

export const compute = (limit: number, digit: number): string => {
  const const_pow_of_10 = 10n ** BigInt((digit - 1) * 2);

  let acc = 0;
  for (const n of range(1, limit + 1)) {
    if (isqrt(n) ** 2 === n) {
      continue;
    }
    // deno-fmt-ignore
    acc += sum(
      String(isqrt(const_pow_of_10 * BigInt(n))).split("").slice(0, digit).map((x) => Number(x)),
    );
  }

  return String(acc);
};

export const solve = (): string => compute(100, 100);
