// project euler: problem 64

/*
            sqrt(N) + b0        1              1
  sqrt(N) = ------------ = a0 + --,  x1 = a1 + --, ...
                 c0             x1             x2

                  c0             c0(sqrt(N) - (b0 - a0c0))
    x1 = --------------------- = -------------------------
         sqrt(N) + (b0 - a0c0)       N - (b0 - a0c0)^2

         sqrt(N) + (a0c0 - b0)   sqrt(N) + b1         1
       = --------------------- = ------------- = a1 + --
           N - (a0c0 - b0)^2          c1              x2
           -----------------
                  c0
   -->
     a{n} = floor( (sqrt(N)+b{n}) / c{n} )
     b{n+1} = a{n}*c{n} - b{n}
     c{n+1} = (N - b{n+1}^2) / c{n}

     b{0} = 0, c{0} = 1, a{0} = sqrt(N)
*/

import { isqrt } from "../lib/math.ts";
import { range } from "../lib/util.ts";

function getCountFraction(n: number): [number, number[]] {
  const trunc = Math.trunc;

  const isqrt_n = isqrt(n);
  if (isqrt_n * isqrt_n === n) {
    return [isqrt_n, []];
  }

  const stopCondition = isqrt_n * 2;
  let b = 0, c = 1;
  let a = trunc((isqrt_n + b) / c);
  const rep: number[] = [];
  while (true) {
    b = a * c - b;
    c = trunc((n - b * b) / c);
    a = trunc((isqrt_n + b) / c);
    rep.push(a);

    // otherwise, c === 1
    if (a === stopCondition) {
      return [isqrt_n, rep];
    }
  }
}

export function compute(limit: number): string {
  let cnt = 0;
  for (const n of range(1, limit + 1)) {
    if (getCountFraction(n)[1].length % 2 === 1) {
      cnt += 1;
    }
  }

  return String(cnt);
}

export function solve(): void {
  const t0 = performance.now();
  const result = compute(10_000);
  const t1 = performance.now();
  const duration_ms = (t1 - t0).toFixed(4);

  console.log(`Answer: ${result}`);
  console.log(`Elapsed time: ${duration_ms} msec.`);

  return;
}
