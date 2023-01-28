
// project euler: problem 26

import { range } from "../lib/util.ts";

function divLoop(a: number, b: number): number {
  const rems = new Map<number, number>();
  let cnt = 0;
  while (true) {
    if (a === 0) {
      return 0;
    }
    if (rems.has(a) === true) {
      return (cnt - (rems.get(a) as number));
    } else {
      rems.set(a, cnt);
      a = (a * 10) % b;
      cnt += 1;
    }
  }
}

export function compute(limit: number): string {
  let maxCycle = 0;
  let n = 0;
  for (const i of range(limit - 1, 1, -1)) {
    if (i <= maxCycle) {
      break;
    }
    const cycleLen = divLoop(1, i);
    if (cycleLen > maxCycle) {
      n = i;
      maxCycle = cycleLen;
    }
  }

  return String(n);
}

export function solve(): void {
  const t0 = performance.now();
  const result = compute(1_000);
  const t1 = performance.now();
  const duration_ms = (t1 - t0).toFixed(4);

  console.log(`Answer: ${result}`);
  console.log(`Elapsed time: ${duration_ms} msec.`);

  return;
}
