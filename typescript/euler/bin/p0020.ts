// project euler: problem 20

import { factorialBigint, sum } from "../lib/math.ts";

export const compute = (n: bigint): string =>
  String(sum(String(factorialBigint(n)).split("").map((x) => Number(x))));

export const solve = (): string => compute(100n);
