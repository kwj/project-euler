// project euler: problem 86

/*
  1 <= a, b, c <= M

  we can ignore rotations. there is only one case to consider.
    1 <= a <= b <= c <= M
     --> 2 <= a + b <= 2c

      +--------F
      |        |      * sqrt(c^2 + (a+b)^2) must be an integer
      |--------|
      |        | a+b >= 2
      |        |
      S--------+
           c

  when a+b <= c <= M
    write a+b = x
      (a, b) = (1, x-1), (2, x-2), ..., (x-1, 1)
    however, because a<=b
      num of (a,b) = floor(x/2) = floor((a+b)/2)

  when a+b > c
      num of (a,b) = floor((a+b)/2) - ((a+b-1) - c)

      example: c=10, a+b=15
        (a,b) = (1,14), ..., (5,10), (6,9), (7,8), ..., (14,1)
                             ####################
                ^^^^^^^^^^^ = (a+b-1) - c
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ = floor((a+b)/2)

      example: c=10, a+b=16
        (a,b) = (1,15), ..., (6,10), (7,9), (8,8), ..., (15,1)
                             ####################
                ^^^^^^^^^^^ = (a+b-1) - c
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ = floor((a+b)/2)
*/

import { isqrt } from "../lib/math.ts";

export function compute(boundary: number): string {
  let acc = 0;
  let c = 3;
  while (acc <= boundary) {
    let ab = c * 2;
    while (ab > 1) {
      const tmp = (c * c) + (ab * ab);
      const tmp_sq = isqrt(tmp);
      if (tmp_sq * tmp_sq === tmp) {
        if (ab <= c) {
          acc += Math.trunc(ab / 2);
        } else {
          acc += Math.trunc(ab / 2) - (ab - 1 - c);
        }
      }
      ab -= 1;
    }
    c += 1;
  }

  return String(c - 1);
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
