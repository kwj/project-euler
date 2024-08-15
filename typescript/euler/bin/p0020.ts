// project euler: problem 20

import { factorial, sum } from "../lib/math.ts";

export const compute = (n: bigint): string =>
  String(sum(String(factorial(n)).split("").map((x) => Number(x))));

export const solve = (): string => compute(100n);
