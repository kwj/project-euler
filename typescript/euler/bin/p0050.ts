// project euler: problem 50

import { isPrime, primeGenerator } from "../lib/prime.ts";

const cumSumGenerator = function* (): Generator<number, void, void> {
  let acc = 0;
  const p_gen = primeGenerator();
  while (true) {
    acc += p_gen.next().value!;
    yield acc;
  }
  // not reached
};

const initCumSumLst = (
  cs_gen: Generator<number, void, void>,
  limit: number,
): number[] => {
  const lst = [0];
  while (lst.at(-1)! < limit) {
    lst.push(cs_gen.next().value!);
  }

  return lst;
};

export const compute = (limit: number): string => {
  if (limit < 3) {
    throw new RangeError("limit must be larger than 2");
  }

  const cs_gen = cumSumGenerator();
  const cs_lst = initCumSumLst(cs_gen, limit);

  let k = cs_lst.length - 2;
  let left = 0;
  while (true) {
    const diff = cs_lst[left + k] - cs_lst[left];
    if (diff >= limit) {
      left = 0;
      k -= 1;
    } else if (isPrime(diff)) {
      return String(diff);
    } else {
      left += 1;
      if (left + k >= cs_lst.length) {
        cs_lst.push(cs_gen.next().value!);
      }
    }
  }
};

export const solve = (): string => compute(1_000_000);
