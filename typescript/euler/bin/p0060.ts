
// project euler: problem 60

/*
  This implementation doesn't set an upper limit of prime numbers and examines them until the smallest
  sum of set is fixed, but it is quite slow. The following is a result on Raspberry Pi 4.

  $ ./solve.sh 60
  Task solve deno run --quiet --allow-hrtime --allow-read --allow-net ./main.ts "60"
  [Problem 60]
  Answer: 26033
  Elapsed time: 55227.6124 msec.

  It is easy to find a 5-clique, however, it needs time to confirm its sum is the smallest.
*/

import { combinations } from "combinatorics/mod.ts";
import { sum } from "../lib/math.ts";
import { isPrime, primeGenerator } from "../lib/primes.ts";
import { range } from "../lib/util.ts";

function isPair(x: number, y: number): boolean {
  function concatNum(a: number, b: number): number {
    let n = 10;
    while (b > n) {
      n *= 10;
    }

    return a * n + b;
  }

  return isPrime(concatNum(x, y)) && isPrime(concatNum(y, x));
}

function findNbrs(prime: number, primeLst: number[], limit: number): number[] {
  return primeLst.filter((x) => x + prime < limit && isPair(x, prime));
}

function isClique(lst: number[], tbl: Map<number, Set<number>>): boolean {
  // The elements in the array 'lst' must be in descending order.
  for (const i of range(0, lst.length - 1)) {
    const nbrs = tbl.get(lst[i]) as Set<number>;
    const target = lst.slice(i + 1);
    for (const x of target) {
      if (nbrs.has(x) === false) {
        return false;
      }
    }
  }

  return true;
}

export function compute(): string {
  // discard 2, 3 and 5
  const p_gen = primeGenerator();
  p_gen.next();
  p_gen.next();
  p_gen.next();

  // Grouping by modulus of 3, but exclude 3. primeSet[0] isn't used.
  // primeSet[1] and primeSet[2] hold prime numbers in descending order.
  const primeSet = [[], [3], [3]];
  const tbl = new Map<number, Set<number>>([[3, new Set([3])]]);
  let answer = Number.MAX_SAFE_INTEGER;

  let prime = 0;
  while ((prime = p_gen.next().value as number) < answer - 792) { // 792 = sum([3, 7, 109, 673])
    const idx = prime % 3;
    const nbrLst = findNbrs(prime, primeSet[idx], answer);
    tbl.set(prime, new Set(nbrLst));
    primeSet[idx].unshift(prime);
    if (nbrLst.length < 4) {
      continue;
    }

    // The combination list which are generated by combinations() are emitted
    // in lexicographic ordering according to the order of the input iterable.
    for (const primeGrp of combinations(nbrLst, 4)) {
      if (sum(prime, ...primeGrp) > answer) {
        continue;
      }
      if (isClique(primeGrp, tbl) === true) {
        answer = Math.min(sum(prime, ...primeGrp), answer);
      }
    }
  }

  return String(answer);
}

export function solve(): void {
  const t0 = performance.now();
  const result = compute();
  const t1 = performance.now();
  const duration_ms = (t1 - t0).toFixed(4);

  console.log(`Answer: ${result}`);
  console.log(`Elapsed time: ${duration_ms} msec.`);

  return;
}