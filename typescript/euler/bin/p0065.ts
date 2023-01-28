
// project euler: problem 65

/*
  e = [2; 1, 2, 1, 1, 4, 1, 1, 6, ..., 1, 1, 2k, ...]
      [a{0}; a{1}, a{2}, ...]

    i  a{i-1}  n(numerator)  d(denominator)
   ----------------------------------------
    1   2         2             1
    2   1         3             1
    3   2         8             3
    4   1        11             4
    5   1        19             7
    6   4        87            32
    7   1       106            39
    8   1       193            71
    9   6      1264           465
   10   1      1457           536
             ...
    i c(i)     n(i)          d(i)

    when i > 2:
      n(i) = n(i-1)*c(i) + n(i-2), n(2) = 3, n(1) = 2
      d(i) = d(i-1)*c(i) + d(i-2), d(2) = 1, d(1) = 1

      c(i) = | 1    (i mod 3 <> 0)
             | 2i/3 (i mod 3 = 0)

  def napier_cf_gen():
      yield 2
      for i in count(2):
          yield 1 if i % 3 != 0 else (2 * i) // 3
*/

import { sum } from "../lib/math.ts";
import { range } from "../lib/util.ts";

function c(i: number): bigint {
  return (i % 3 !== 0) ? 1n : (2n * BigInt(i)) / 3n;
}

export function compute(nth: number): string {
  let n_i1 = 3n, n_i2 = 2n;
  for (const i of range(3, nth + 1)) {
    [n_i1, n_i2] = [n_i1 * c(i) + n_i2, n_i1];
  }

  return String(sum(String(n_i1).split("").map((x) => Number(x))));
}

export function solve(): void {
  const t0 = performance.now();
  const result = compute(100);
  const t1 = performance.now();
  const duration_ms = (t1 - t0).toFixed(4);

  console.log(`Answer: ${result}`);
  console.log(`Elapsed time: ${duration_ms} msec.`);

  return;
}
