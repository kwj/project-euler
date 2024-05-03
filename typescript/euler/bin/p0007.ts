// project euler: problem 7

import { primeGenerator } from "../lib/primes.ts";

export const compute = (limit: number): string => {
  let answer = 0;
  const p_gen = primeGenerator();

  for (let i = 0; i < limit; i++) {
    answer = p_gen.next().value as number;
  }

  return String(answer);
};

export const solve = (): string => compute(10_001);
