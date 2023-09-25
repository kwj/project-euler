// project euler: problem 87

/*
  >>> 50000000 ** (1/2)
  7071.067811865475
  >>> 50000000 ** (1/3)
  368.40314986403854
  >>> 50000000 ** (1/4)
  84.08964152537145
*/

import { takeWhile } from "std/collections/take_while.ts";
import { getPrimes } from "../lib/primes.ts";

export function compute(limit: number): string {
  const sqPlst = getPrimes(Math.pow(limit - 2 ** 3 - 2 ** 4, 1 / 2));
  const cbPlst = takeWhile(sqPlst, (x) => x <= Math.pow(limit, 1 / 3));
  const fthPlst = takeWhile(sqPlst, (x) => x <= Math.pow(limit, 1 / 4));

  const result = new Set<number>();
  for (const z of fthPlst) {
    for (const y of cbPlst) {
      for (const x of sqPlst) {
        const tmp = x ** 2 + y ** 3 + z ** 4;
        if (tmp < limit) {
          result.add(tmp);
        }
      }
    }
  }

  return String(result.size);
}

export function solve(): void {
  const t0 = performance.now();
  const result = compute(50_000_000);
  const t1 = performance.now();
  const duration_ms = (t1 - t0).toFixed(4);

  console.log(`Answer: ${result}`);
  console.log(`Elapsed time: ${duration_ms} msec.`);

  return;
}
