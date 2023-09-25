
// project euler: problem 2

/*
  f₁ = 1, f₂ = 2, f₃ = 3, f₄ = 5, f₅ = 8, f₆ = 13, f₇ = 21, f₈ = 34, f₉ = 55, ...

  assume that k ≥ 7
    f(k) = f(k-1) + f(k-2)
         = 2f(k-2) + f(k-3)
         = 2(f(k-3) + f(k-4)) + f(k-3)
         = 3f(k-3) + 2f(k-4)
         = 3f(k-3) + 2f(k-5) + 2f(k-6)
         = 4f(k-3) - f(k-3) + 2f(k-5) + 2f(k-6)
         = 4f(k-3) - (f(k-4) + f(k-5)) + 2f(k-5) + 2f(k-6)
         = 4f(k-3) - f(k-4) + f(k-5) + 2f(k-6)
         = 4f(k-3) - f(k-4) + (f(k-5) + f(k-6)) + f(k-6)
         = 4f(k-3) - f(k-4) + f(k-4) + f(k-6)
         = 4f(k-3) + f(k-6)
*/

function* evenFibGen() {
  let a = 2, b = 8;
  while (true) {
    yield a;
    [a, b] = [b, 4 * b + a];
  }
}

export function compute(limit: number): string {
  const fib_gen = evenFibGen();
  let result = 0;

  for (const n of fib_gen) {
    if (n > limit) {
      break;
    }
    result += n;
  }

  return String(result);
}

export function solve(): void {
  const t0 = performance.now();
  const result = compute(4_000_000);
  const t1 = performance.now();
  const duration_ms = (t1 - t0).toFixed(4);

  console.log(`Answer: ${result}`);
  console.log(`Elapsed time: ${duration_ms} msec.`);

  return;
}
