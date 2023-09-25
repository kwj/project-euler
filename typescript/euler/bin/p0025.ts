// project euler: problem 25

export function compute(digit: number): string {
  const limit = 10n ** (BigInt(digit) - 1n);
  let nth = 2;
  let fib1 = 1n, fib2 = 1n;
  while (fib2 < limit) {
    nth += 1;
    [fib1, fib2] = [fib2, fib1 + fib2];
  }

  return String(nth);
}

export function solve(): void {
  const t0 = performance.now();
  const result = compute(1_000);
  const t1 = performance.now();
  const duration_ms = (t1 - t0).toFixed(4);

  console.log(`Answer: ${result}`);
  console.log(`Elapsed time: ${duration_ms} msec.`);

  return;
}
