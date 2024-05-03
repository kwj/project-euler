// project euler: problem 24

import { factorial } from "../lib/math.ts";
import { range } from "../lib/util.ts";

const trunc = Math.trunc;

function makeFactTbl(n: number, depth: number): number[] {
  const result: number[] = [];
  let i = n - 1;
  const divisor = factorial(n - depth);
  for (const _ of range(0, depth)) {
    result.push(trunc(factorial(i) / divisor));
    i -= 1;
  }

  return result;
}

export const compute = (nth: number, elmLst: number[]): string => {
  const depth = elmLst.length;
  let idx = nth - 1;
  const result: number[] = [];
  for (const n of makeFactTbl(elmLst.length, depth)) {
    result.push(elmLst[trunc(idx / n)]);
    elmLst.splice(trunc(idx / n), 1);
    idx %= n;
  }

  return result.map((x) => String(x)).join("");
};

export const solve = (): string => compute(1_000_000, range(0, 10));
