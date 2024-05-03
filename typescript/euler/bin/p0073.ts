// project euler: problem 73

/*
  f(n): number of fractions a/b, where a < b, b <= n, 1/3 < a/b < 1/2
        --> sigma{i=1, ...,n}((i-1)//2 - i//3)
  g(n): number of irreducible fractions a/b, where a < b, b <= n, 1/3 < a/b < 1/2, gcd(a,b)=1

    The answer we should seek is g(12000).

  f(n) = sigma{k=1, ..., n}(g(n//k))
  -->
    g(n) = sigma{k=1, ..., n}μ(k)f(n//k)      [möbius inversion formula, μ(): möbius function]
        = sigma{k=1, ..., n}μ(k)sigma{j=1, ..., n//k}((j-1)//2 - j//3)
*/

import { isqrt, sum } from "../lib/math.ts";
import { range } from "../lib/util.ts";

const trunc = Math.trunc;

const makeMobiusTbl = (limit: number): number[] => {
  const p_tbl = range(0, limit + 1);
  for (const i of range(2, isqrt(limit) + 1)) {
    if (p_tbl[i] === i) {
      for (const j of range(i * i, limit + 1, i)) {
        p_tbl[j] = i;
      }
      for (const j of range(i * i, limit + 1, i * i)) {
        p_tbl[j] = 0;
      }
    }
  }

  const mu_tbl: number[] = new Array(limit + 1).fill(0);
  mu_tbl[1] = 1;
  for (const i of range(2, limit + 1)) {
    if (p_tbl[i] != 0) {
      mu_tbl[i] = -mu_tbl[trunc(i / p_tbl[i])];
    }
  }

  return mu_tbl;
};

const f = (x: number): number =>
  sum(range(1, x + 1).map((j) => trunc((j - 1) / 2) - trunc(j / 3)));

const g = (N: number): number => {
  const mu_tbl = makeMobiusTbl(N);
  return sum(range(1, N + 1).map((k) => mu_tbl[k] * f(trunc(N / k))));
};

export const compute = (upper: number): string => String(g(upper));

export const solve = (): string => compute(12_000);
