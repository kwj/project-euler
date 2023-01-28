
// project euler: problem 38

/*
  It is clear that number X is within 4 digits from the problem statement.

    1) if number X is four digits, n = 2  (X * 1 -> 4-digits, X * 2 -> 5-digits)
    2) if number X is three digits, n = 3  (X * 1 -> 3-digits, X * 2 -> 3-digits, X * 3 -> 3-digits)
    3) if number X is two digits, n = 4  (X * 1 -> 2-digits, X * 2 -> 2-digits, X * 3 -> 2-digits, X * 4 -> 3-digits)
    4) if number X is one digit, n = 9 or 5 (only X=1 and n=9, X=9 and n=5).

  case #1:
    5000 <= X <= 9999
  case #2:
    100 <= X <= 333
  case #3:
    10 <= X <= 33
  case #4:
    X = 1, 9
*/

import { range } from "../lib/util.ts";

export function compute(): string {
  const lst: string[] = [];
  const specs: [number, [number, number]][] = [[2, [5_000, 10_000]], [3, [100, 334]], [4, [10, 34]], [5, [9, 10]], [9, [1, 2]]];

  for (const [n, spec] of specs) {
    for (const x of range(...spec)) {
      const s = range(1, n + 1).map((k) => String(x * k)).reduce((acc, cur) => acc + cur);
      if (s.split("").sort().join("") === "123456789") {
        lst.push(s);
      }
    }
  }

  return lst.sort().reverse()[0];
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
