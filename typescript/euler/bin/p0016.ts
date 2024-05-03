// project euler: problem 16

import { sum } from "../lib/math.ts";

export const compute = (exp: bigint): string =>
  String(sum(String(2n ** exp).split("").map((x) => Number(x))));

export const solve = (): string => compute(1000n);
