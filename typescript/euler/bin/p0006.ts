// project euler: problem 6

import { sum } from "../lib/math.ts";
import { range } from "../lib/util.ts";

export const compute = (upper: number): string => {
  const sequence = range(1, upper + 1);

  // The square of sum is equal or larger than the sum of squares.
  return String(sum(sequence) ** 2 - sum(sequence.map((n) => n * n)));
};

export const solve = (): string => compute(100);
