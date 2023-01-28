
// project euler: problem 50

import { dropWhile } from "std/collections/drop_while.ts";
import { isPrimeSimple, primeGenerator } from "../lib/primes.ts";

function* cumSumGenerator(): Generator<number, void, void> {
  let acc = 0;
  const p_gen = primeGenerator();
  while (true) {
    acc += p_gen.next().value as number;
    yield acc;
  }
  // not reached
}

function initCumSumLst(cs_gen: Generator<number, void, void>, limit: number): number[] {
  const lst = [0];
  while (lst.at(-1)! < limit) {
    lst.push(cs_gen.next().value as number);
  }

  return lst;
}

export function compute(limit: number): string {
  const cs_gen = cumSumGenerator();
  const cs_lst = initCumSumLst(cs_gen, limit);

  let ans = 0;
  let i = 0;
  let width = 1;
  while (cs_lst[i + width] - cs_lst[i] < limit) {
    const begin = cs_lst[i];
    const lst = dropWhile(cs_lst.slice(i + width).reverse(), (p) => p - begin >= limit || isPrimeSimple(p - begin) === false);
    if (lst.length > 0) {
      width += lst.length;
      ans = lst[0] - begin;
    }
    cs_lst.concat(cs_gen.next().value as number);
    i += 1;
  }

  return String(ans);
}

export function solve(): void {
  const t0 = performance.now();
  const result = compute(1_000_000);
  const t1 = performance.now();
  const duration_ms = (t1 - t0).toFixed(4);

  console.log(`Answer: ${result}`);
  console.log(`Elapsed time: ${duration_ms} msec.`);

  return;
}
