// project euler: problem 50

import { dropWhile } from "@std/collections";
import { isPrimeSimple, primeGenerator } from "../lib/primes.ts";

const cumSumGenerator = function* (): Generator<number, void, void> {
  let acc = 0;
  const p_gen = primeGenerator();
  while (true) {
    acc += p_gen.next().value as number;
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
    lst.push(cs_gen.next().value as number);
  }

  return lst;
};

export const compute = (limit: number): string => {
  const cs_gen = cumSumGenerator();
  const cs_lst = initCumSumLst(cs_gen, limit);

  let ans = 0;
  let i = 0;
  let consecLength = 0;
  while (cs_lst[i + consecLength] - cs_lst[i] < limit) {
    const begin = cs_lst[i];
    const lst = dropWhile(
      cs_lst.slice(i + consecLength).reverse(),
      (p) => p - begin >= limit || isPrimeSimple(p - begin) === false,
    );
    if (lst.length > 0) {
      consecLength += lst.length - 1;
      ans = lst[0] - begin;
    }
    cs_lst.push(cs_gen.next().value as number);
    i += 1;
  }

  return String(ans);
};

export const solve = (): string => compute(1_000_000);
