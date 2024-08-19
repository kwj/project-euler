/*
  Prime factorization and more
 */

import { cartesianProduct } from "combinatorics/mod.ts";
import { isqrt, prod } from "./math.ts";
import { minFactorTbl, primeGenerator, primes } from "./prime.ts";
import { range } from "./util.ts";
import { unzip } from "@std/collections";

export const primeFactors = (n: number): number[] => {
  if (n < 1) {
    throw new Error("parameter is too small");
  } else if (n === 1) {
    return [1];
  }

  const result: number[] = [];
  for (const b of [2, 3, 5]) {
    while (n % b === 0) {
      result.push(b);
      n = Math.trunc(n / b);
    }
  }

  // 7, 11, 13, 17, 19, 23, 29, 31, (37, ...)
  const diff = [4, 2, 4, 2, 4, 6, 2, 6];
  let b = 7;
  let idx = 0;
  const limit = isqrt(n);

  while (b <= limit) {
    while (n % b === 0) {
      result.push(b);
      n = Math.trunc(n / b);
    }
    b += diff[idx];
    idx = (idx + 1) % 8;
  }

  if (n != 1) {
    result.push(n);
  }

  return result;
};

const group = (lst: number[]): number[][] => {
  const result: number[][] = [];
  let prev = lst[0];
  let tmp: number[] = [prev];

  for (const x of lst.slice(1)) {
    if (x === prev) {
      tmp.push(x);
    } else {
      result.push(tmp);
      prev = x;
      tmp = [x];
    }
  }
  result.push(tmp);

  return result;
};

export const primeFactorization = (n: number): [number, number][] =>
  group(primeFactors(n)).map((lst) => [lst[0], lst.length]);

export const pfactorsToDivisors = (lst: number[]): number[] => {
  const prodScan = (l: number[]): number[] => {
    const result: number[] = [];
    let x = 1;
    for (const v of l) {
      x *= v;
      result.push(x);
    }
    return result;
  };

  return group(lst).map((l) => prodScan(l)).reduce(
    (l1: number[], l2: number[]) =>
      l1.concat([...cartesianProduct(l1, l2)].map((x) => prod(x))),
    [1],
  );
};

export const divisors = (n: number): number[] => {
  return pfactorsToDivisors(primeFactors(n));
};

export const numOfDivisors = (n: number): number => {
  const [_, e]: [number[], number[]] = unzip(primeFactorization(n));
  return e.map((x) => x + 1)
    .reduce((acc, cur) => acc * cur, 1);
};

export const minFactor = (n: number): number => {
  if (n < 1) {
    throw new RangeError("argument must be a positive number.");
  } else if (n % 2 === 0) {
    return 2;
  } else if (n <= 65535) {
    let tmp: number;
    return (tmp = minFactorTbl[n >> 1]) === 1 ? n : tmp;
  } else {
    const p_gen = primeGenerator(3);
    while (true) {
      const p = p_gen.next().value!;
      if (n % p === 0) {
        return p;
      }
    }
  }
};

export const sigmaTbl = (z: number, limit: number): number[] => {
  const pLst = primes(limit);
  const tbl: number[] = Array(limit + 1).fill(1);

  let q: number;
  let x: number;
  for (const p of pLst) {
    q = p;
    x = 0;
    while (q <= limit) {
      x += q ** z;
      tbl[q] += x;
      q *= p;
    }
  }
  for (const p of pLst) {
    q = p;
    while (q <= limit) {
      for (const n of range(2, Math.trunc(limit / q) + 1)) {
        if (tbl[n] === 1 || n % p === 0) {
          continue;
        }
        tbl[q * n] = tbl[q] * tbl[n];
      }
      q *= p;
    }
  }
  tbl[0] = 0;

  return tbl;
};

export const aliquotSumTbl = (limit: number): number[] => {
  const tbl = sigmaTbl(1, limit);
  for (const i of range(1, limit + 1)) {
    tbl[i] -= i;
  }

  return tbl;
};
