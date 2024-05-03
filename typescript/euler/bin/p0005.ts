// project euler: problem 5

import { lcm } from "../lib/math.ts";
import { range } from "../lib/util.ts";

export const compute = (upper: number): string =>
  String(lcm(range(1, upper + 1)));

export const solve = (): string => compute(20);
