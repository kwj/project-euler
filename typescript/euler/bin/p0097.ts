// project euler: problem 97

import { modPow } from "../lib/math.ts";

export const compute = (): string => {
  const modulas = 10_000_000_000n;

  return String((28433n * modPow(2n, 7830457n, modulas) + 1n) % modulas);
};

export const solve = (): string => compute();
