
// project euler: problem 47

import { factorize } from "../lib/math.ts";

export function compute(nfactors: number): string {
  let cnt = 0;

  for (let x = 1;; x++) {
    if (factorize(x).length !== nfactors) {
      cnt = 0;
    } else if (cnt === nfactors - 1) {
      return String(x - (nfactors - 1));
    } else {
      cnt += 1;
    }
  }
}

export function solve(): void {
  const t0 = performance.now();
  const result = compute(4);
  const t1 = performance.now();
  const duration_ms = (t1 - t0).toFixed(4);

  console.log(`Answer: ${result}`);
  console.log(`Elapsed time: ${duration_ms} msec.`);

  return;
}
