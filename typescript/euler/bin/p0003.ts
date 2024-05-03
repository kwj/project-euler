// project euler: problem 3

import { factorize } from "../lib/math.ts";

export const compute = (n: number): string => String(factorize(n).at(-1)![0]);

export const solve = (): string => compute(600_851_475_143);
