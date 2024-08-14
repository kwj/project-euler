// project euler: problem 49

import { combinations } from "combinatorics/mod.ts";
import { isPrime, primes } from "../lib/prime.ts";

export const compute = (limit: number): string => {
  const isPerm = (p1: number, p2: number, p3: number): boolean => {
    const s1 = String(p1).split("").sort().join("");
    const s2 = String(p2).split("").sort().join("");
    const s3 = String(p3).split("").sort().join("");

    return s1 === s2 && s1 === s3;
  };

  const primeLst = primes(1000, limit);
  for (const [i, j] of combinations(primeLst, 2)) {
    const m = Math.trunc((i + j) / 2);
    if (isPrime(m) === true && isPerm(i, m, j) === true) {
      if (i + j === 1487 + 8147) {
        continue;
      }
      return String(i) + String(m) + String(j);
    }
  }

  throw new Error("not reached");
};

export const solve = (): string => compute(9_999);
