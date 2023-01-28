
// project euler: problem 53

import { factorial, sum } from "../lib/math.ts";
import { range } from "../lib/util.ts";

function comb(n: number, r: number) {
  return Math.trunc(Math.trunc(factorial(n) / factorial(r)) / factorial(n - r));
}

function find_r(n: number, r: number, boundary: number): number | undefined {
  const crnt = comb(n, r);
  if (crnt > boundary) {
    // search to the left
    while (comb(n, r - 1) > boundary) {
      r -= 1;
    }
    return r;
  } else {
    // search to the right
    let right = comb(n, r + 1);
    while (right <= boundary) {
      if (crnt > right) {
        return undefined;
      }
      r += 1;
      right = comb(n, r + 1);
    }
    return r + 1;
  }
}

export function compute(num: number, boundary: number): string {
  let r = 0;
  while (comb(num, r) <= boundary) {
    r += 1;
  }

  const lst: [number, number][] = [];
  for (const n of range(num, 0, -1)) {
    const next_r = find_r(n, r, boundary);
    if (next_r === undefined) {
      break;
    }
    lst.push([n, next_r]);
    r = next_r;
  }

  return String(sum(lst.map((x) => x[0] + 1 - 2 * x[1])));
}

export function solve(): void {
  const t0 = performance.now();
  const result = compute(100, 1_000_000);
  const t1 = performance.now();
  const duration_ms = (t1 - t0).toFixed(4);

  console.log(`Answer: ${result}`);
  console.log(`Elapsed time: ${duration_ms} msec.`);

  return;
}
