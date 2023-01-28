
// project euler: problem 31

import { range } from "../lib/util.ts";

export function compute(coins: number[], target: number): string {
  const tbl = new Array(target + 1);
  tbl.fill(0);
  tbl[0] = 1;

  for (const c of coins) {
    for (const i of range(c, target + 1)) {
      tbl[i] += tbl[i - c];
    }
  }

  return String(tbl[target]);
}

export function solve(): void {
  const t0 = performance.now();
  const result = compute([1, 2, 5, 10, 20, 50, 100, 200], 200);
  const t1 = performance.now();
  const duration_ms = (t1 - t0).toFixed(4);

  console.log(`Answer: ${result}`);
  console.log(`Elapsed time: ${duration_ms} msec.`);

  return;
}
