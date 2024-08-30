// project euler: problem 26

import { range } from "../lib/util.ts";

const divLoop = (a: number, b: number): number => {
  const rems = new Map<number, number>();
  let cnt = 0;
  while (true) {
    if (a === 0) {
      return 0;
    }
    if (rems.has(a)) {
      return (cnt - (rems.get(a)!));
    } else {
      rems.set(a, cnt);
      a = (a * 10) % b;
      cnt += 1;
    }
  }
};

export const compute = (limit: number): string => {
  let maxCycle = 0;
  let n = 0;
  for (const i of range(Math.trunc(limit / 2), limit).toReversed()) {
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
};

export const solve = (): string => compute(1_000);
