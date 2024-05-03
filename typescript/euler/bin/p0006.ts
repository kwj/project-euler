// project euler: problem 6

import { sum } from "../lib/math.ts";
import { range } from "../lib/util.ts";

export const compute = (upper: number): string => {
  const sequence = range(1, upper + 1);

  return String(Math.abs(sum(sequence.map((n) => n * n)) - sum(sequence) ** 2));
};

export const solve = (): string => compute(100);
