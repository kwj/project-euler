// project euler: problem 77

import { primeGenerator } from "../lib/primes.ts";
import { range } from "../lib/util.ts";

const plstGenerator = function* (): Generator<number[], void, unknown> {
  const p_gen = primeGenerator();
  const plst: number[] = [];
  while (true) {
    plst.push(p_gen.next().value as number);
    yield plst;
  }
};

export const compute = (boundary: number): string => {
  const plst_gen = plstGenerator();
  let plst: number[] = [];

  while (true) {
    plst = plst_gen.next().value as number[];
    const tbl: number[] = new Array(plst.length + 1).fill(0);
    tbl[0] = 1;
    for (const i of plst) {
      for (const j of range(i, tbl.length)) {
        tbl[j] += tbl[j - i];
      }
    }

    if (tbl.at(-1)! > boundary) {
      break;
    }
  }

  return String(plst.length);
};

export const solve = (): string => compute(5_000);
