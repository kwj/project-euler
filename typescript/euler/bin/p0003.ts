// project euler: problem 3

import { primeFactors } from "../lib/factor.ts";

export const compute = (n: number): string => String(primeFactors(n).at(-1));

export const solve = (): string => compute(600_851_475_143);
