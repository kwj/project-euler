// project euler: problem 15

import { factorialBigint } from "../lib/math.ts";

export const compute = (m: number, n: number): string => {
  return String(
    factorialBigint(BigInt(m + n)) / factorialBigint(BigInt(m)) /
      factorialBigint(BigInt(n)),
  );
};

export const solve = (): string => compute(20, 20);
