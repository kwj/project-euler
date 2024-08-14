// project euler: problem 10

import { sum } from "../lib/math.ts";
import { primes } from "../lib/prime.ts";

export const compute = (limit: number): string => String(sum(primes(limit)));

export const solve = (): string => compute(2_000_000);
