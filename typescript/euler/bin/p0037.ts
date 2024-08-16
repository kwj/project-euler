// project euler: problem 37

/*
  candidate numbers: [2357][1379]*[37]
                           ----------- lst
*/

import { sum } from "../lib/math.ts";
import { isPrime } from "../lib/prime.ts";
import { numOfDigits } from "../lib/util.ts";

const addPrefix = (prefixes: number[], lst: number[]): number[] => {
  const acc: number[] = [];
  for (const n of lst) {
    for (const p of prefixes) {
      acc.push(p * (10 ** numOfDigits(n)) + n);
    }
  }

  return acc;
};

const makeNextLst = (lst: number[]): number[] =>
  addPrefix([1, 3, 7, 9], lst).filter((x) => isPrime(x));

const pickupPrimes = (lst: number[]): number[] => {
  const isTruncablePrime = (n: number): boolean => {
    if (n === 0) {
      return false;
    }
    while (n !== 0) {
      if (!isPrime(n)) {
        return false;
      }
      n = Math.trunc(n / 10);
    }

    return true;
  };

  return addPrefix([2, 3, 5, 7], lst).filter((x) => isTruncablePrime(x));
};

export const compute = (): string => {
  let result: number[] = [];
  let lst = [3, 7];
  while (result.length < 11) {
    result = result.concat(pickupPrimes(lst));
    lst = makeNextLst(lst);
  }

  return String(sum(result));
};

export const solve = (): string => compute();
