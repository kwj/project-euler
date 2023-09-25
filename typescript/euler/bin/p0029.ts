// project euler: problem 29

import { getMaxExp, isqrt, lcm, sum } from "../lib/math.ts";
import { range } from "../lib/util.ts";

function make_dupctr_tbl(upper: number): number[] {
  const max_exp: number = getMaxExp(upper, 2);
  const dup_ctr: number[] = new Array(max_exp + 1);
  const dups: number[] = new Array(upper + 1);

  for (const x of range(2, max_exp + 1)) {
    dups.fill(0);
    for (const y of range(1, x)) {
      const k = lcm(x, y) / x;
      for (let i = Math.max(k, 2); i <= (upper * y / x); i += k) {
        dups[i] = 1;
      }
    }
    dup_ctr[x] = sum(dups);
  }

  return dup_ctr;
}

export function compute(upper: number): string {
  const dup_ctr: number[] = make_dupctr_tbl(upper);
  const base_limit: number = isqrt(upper);
  const skip_flag: boolean[] = new Array(base_limit + 1);
  skip_flag.fill(false);

  let ans = (upper - 1) ** 2;
  for (const b of range(2, base_limit + 1)) {
    if (skip_flag[b] === true) {
      continue;
    }
    for (const e of range(2, getMaxExp(upper, b) + 1)) {
      ans -= dup_ctr[e];
      const tmp = b ** e;
      if (tmp <= base_limit) {
        skip_flag[tmp] = true;
      }
    }
  }

  return String(ans);
}

export function solve(): void {
  const t0 = performance.now();
  const result = compute(100);
  const t1 = performance.now();
  const duration_ms = (t1 - t0).toFixed(4);

  console.log(`Answer: ${result}`);
  console.log(`Elapsed time: ${duration_ms} msec.`);

  return;
}
