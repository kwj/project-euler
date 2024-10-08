// project euler: problem 87

/*
  >>> 50000000 ** (1/2)
  7071.067811865475
  >>> 50000000 ** (1/3)
  368.40314986403854
  >>> 50000000 ** (1/4)
  84.08964152537145
*/

import { takeWhile } from "@std/collections";
import { primes } from "../lib/prime.ts";

export const compute = (limit: number): string => {
  const sqPlst = primes(Math.pow(limit - 2 ** 3 - 2 ** 4, 1 / 2));
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
};

export const solve = (): string => compute(50_000_000);
