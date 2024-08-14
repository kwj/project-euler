// project euler: problem 97

import { modPow } from "../lib/math.ts";

export const compute = (): string => {
  const modulas = 10_000_000_000;

  return String((28433 * modPow(2, 7830457, modulas) + 1) % modulas);
};

export const solve = (): string => compute();
