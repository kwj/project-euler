// project euler: problem 72

/*
  See https://mathproblems123.wordpress.com/2018/05/10/sum-of-the-euler-totient-function/
*/

import { isqrt } from "../lib/math.ts";
import { range } from "../lib/util.ts";

const sumPhi = (n: number, memo: Map<string, number> = new Map()): number => {
  const trunc = Math.trunc;

  const key = String(n);
  if (memo.has(key)) {
    return memo.get(key)!;
  }

  let v = trunc(n * (n + 1) / 2);
  for (const m of range(2, isqrt(n) + 1)) {
    v -= sumPhi(trunc(n / m), memo);
  }
  for (const d of range(1, trunc(n / (isqrt(n) + 1)) + 1)) {
    v -= (trunc(n / d) - trunc(n / (d + 1))) * sumPhi(d, memo);
  }
  memo.set(key, v);

  return v;
};

export const compute = (limit: number): string =>
  String(sumPhi(limit) - sumPhi(1));

export const solve = (): string => compute(1_000_000);
