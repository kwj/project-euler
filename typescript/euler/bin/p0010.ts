// project euler: problem 10

import { sum } from "../lib/math.ts";
import { getPrimes } from "../lib/primes.ts";

export const compute = (limit: number): string => String(sum(getPrimes(limit)));

export const solve = (): string => compute(2_000_000);
