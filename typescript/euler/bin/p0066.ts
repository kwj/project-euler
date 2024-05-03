// project euler: problem 66

/*
  X^2 - N * Y^2 = 1
  -----------------

  This equation is called Pell's equation, but I didn't know that.
  So I wrote a solution by referring Wikipedia.

    https://en.wikipedia.org/wiki/Pell%27s_equation


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
     b{n+1} = a{n}*c{n} - b{n}
     c{n+1} = (N - b{n+1}^2) / c{n}
     a{n+1} = floor((sqrt(N)+b{n+1}) / c{n+1}) = (floor(sqrt(N)) + b{n+1}) / c{n+1}

       a{0} = floor(sqrt(N)), b{0} = 0, c{0} = 1


  write A{0}, A{1}, A{2}, ...
           x{0}                          x{1}                      1         x{2}
    a{0} = ---- = A{0},  a{0} + 1/a{1} = ---- = A{1},  a{0} + ------------ = ---- = A{2},  ...
           y{0}                          y{1}                         1      y{2}
                                                              a{1} + ----
                                                                     a{2}
                                              [x{0} = a{0}, y{0} = 1]
   -->
        n=0: -> x{0} = a{0}, y{0} = 1
        n=1: -> x{1} = a{0}*a{1} + 1, y{1} = a{1}

        n=2: -> x{2}/y{2}
                     = (a{0}*a{1}*a{2} + a{0} + a{2}) / (a{1}a{2} + 1)

                       a{2}*(a{0}*a{1} + 1) + a{0}   a{2}*x{1} + a{0}
                     = --------------------------- = ----------------
                            a{2}*a{1} + 1            a{2}*y(1) + 1

                       a{2}*x{1} + x{0}
                     = ----------------
                       a{2)*y{1} + y{0}

                                    a{k}*x{k-1} + x{k-2}
     assume that A{k} = x{k}/y{k} = --------------------  [k>=2]
                                    a{k}*y{k-1} + y{k-2}

                                 ((a{k}*a{k+1} + 1)/a{k+1})*x{k-1} + x{k-2}
        A{k+1} = x{k+1}/y{k+1} = -----------------------------------------
                                 ((a{k}*a{k+1} + 1)/a{k+1})*y{k-1} + y{k-2}

                                 (a{k}*a{k+1} + 1)*x{k-1} + x{k-2}*a{k+1}
                               = -----------------------------------------
                                 (a{k}*a{k+1} + 1)*y{k-1} + y{k-2}*a{k+1}

                                 a{k+1}(a{k}*x{k-1} + x{k-2}) + x{k-1}
                               = -------------------------------------
                                 a{k+1}(a{k}*y{k-1} + y{k-2}) + y{k-1}

                                 a{k+1}*x{k} + x{k-1}
                               = --------------------
                                 a{k+1}*y{k} + y{k-1}
      -->
        x{k+1} = a{k+1} * x{k} + x{k-1}
        y{k+1} = a{k+1} * y{k} + y{k-1}

   -->
     [a{0}; a{1], a{2}, ...]
     assume that x{-1} = 1, x{0} = a{0}, y{-1} = 0, y{0} = 1

     [n>=1]
       x{n} = a{n} * x{n-1} + x{n-2}
       y{n} = a{n} * y{n-1} + y{n-2}
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

function getNumerator(a0: number, repLst: number[]): number {
  let x_n1 = a0, x_n2 = 1;
  for (const a of repLst) {
    [x_n1, x_n2] = [a * x_n1 + x_n2, x_n1];
  }

  return x_n1;
}

export const compute = (limit: number): string => {
  let answer: [number, number] = [0, 0];
  for (const i of range(1, limit + 1)) {
    const cf = getCountFraction(i);
    if (cf[1].length === 0) {
      continue;
    }

    let numerator = 0;
    if (cf[1].length % 2 === 0) {
      numerator = getNumerator(cf[0], cf[1].slice(0, -1));
    } else {
      numerator = getNumerator(cf[0], cf[1].concat(cf[1]).slice(0, -1));
    }
    if (answer[0] < numerator) {
      answer = [numerator, i];
    }
  }

  return String(answer[1]);
};

export const solve = (): string => compute(1_000);
