
// project euler: problem 77

import { primeGenerator } from "../lib/primes.ts";
import { range } from "../lib/util.ts";

function* plstGenerator(): Generator<number[], void, unknown> {
  const p_gen = primeGenerator();
  const plst: number[] = [];
  while (true) {
    plst.push(p_gen.next().value as number);
    yield plst;
  }
}

export function compute(boundary: number): string {
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
}

export function solve(): void {
  const t0 = performance.now();
  const result = compute(5_000);
  const t1 = performance.now();
  const duration_ms = (t1 - t0).toFixed(4);

  console.log(`Answer: ${result}`);
  console.log(`Elapsed time: ${duration_ms} msec.`);

  return;
}
