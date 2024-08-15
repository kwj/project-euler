// project euler: problem 15

import { factorial } from "../lib/math.ts";

export const compute = (m: number, n: number): string => {
  return String(
    factorial(BigInt(m + n)) / factorial(BigInt(m)) / factorial(BigInt(n)),
  );
};

export const solve = (): string => compute(20, 20);
