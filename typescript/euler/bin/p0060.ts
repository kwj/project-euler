// project euler: problem 60

/*
  This implementation assumes an upper limit of prime numbers as MAX_SAFE_INTEGER
  and also confirms the answer is the smallest sum. So, it is a bit slow.
  The following is a result with Deno 1.45.5 on Raspberry Pi 4.

  $ ./solve.sh 60
  Task solve deno run --quiet --allow-hrtime --allow-read --allow-net ./main.ts "60"
  [Problem 60]
  Answer: 26033
  Elapsed time: 4795.8085 msec.

  It is easy to find a 5-clique, however, it needs time to confirm its sum is the smallest.
*/

import { minLst, sum } from "../lib/math.ts";
import { isPrime, primeGenerator } from "../lib/prime.ts";
import { numOfDigits, range } from "../lib/util.ts";

const isPair = (
  a: number,
  upper_a: number,
  b: number,
  upper_b: number,
): boolean => isPrime(a * upper_b + b) && isPrime(b * upper_a + a);

const findNbrs = (
  prime: number,
  ascPrimeLst: number[],
  limit: number,
): number[] => {
  const upper_prime = 10 ** numOfDigits(prime);
  let upper_x = 10;
  const result: number[] = [];
  for (const x of ascPrimeLst) {
    if (x + prime >= limit) {
      break;
    }
    while (x > upper_x) {
      upper_x *= 10;
    }
    if (isPair(prime, upper_prime, x, upper_x)) {
      result.push(x);
    }
  }

  return result;
};

const findCliques = (
  ascNbrs: number[],
  size: number,
  tbl: Map<number, Set<number>>,
): number[][] => {
  const aux = (clq: number[], lst: number[], depth: number): void => {
    if (depth === 0) {
      result.push(clq);
    } else {
      for (const i of range(0, lst.length - depth + 1)) {
        if (clq.length === 0 || clq.every((x) => tbl.get(x)!.has(lst[i]))) {
          aux(clq.concat([lst[i]]), lst.slice(i + 1), depth - 1);
        }
      }
    }
  };

  const result: number[][] = [];
  aux([], ascNbrs.toReversed(), size);

  return result;
};

export const compute = (groupSize: number): string => {
  // Start with 7 (discard 2, 3 and 5)
  const p_gen = primeGenerator(7);

  // primeSet[0] isn't used.
  // primeSet[1] and primeSet[2] hold prime numbers in ascending order.
  const primeSet = [[], [3], [3]];
  const tbl = new Map<number, Set<number>>();
  let answer = Number.MAX_SAFE_INTEGER;

  let prime: number;
  while ((prime = p_gen.next().value!) < answer) {
    const idx = prime % 3;
    const nbrLst = findNbrs(prime, primeSet[idx], answer);
    tbl.set(prime, new Set(nbrLst));
    primeSet[idx].push(prime);
    if (nbrLst.length < groupSize - 1) {
      continue;
    }

    const cliques = findCliques(nbrLst, groupSize - 1, tbl);
    if (cliques.length > 0) {
      const tmp = minLst(cliques.map((lst) => prime + sum(lst)));
      if (tmp < answer) {
        answer = tmp;
      }
    }
  }

  return String(answer);
};

export const solve = (): string => compute(5);
