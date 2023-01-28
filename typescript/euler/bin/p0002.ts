
// project euler: problem 2

function* fibGen() {
  let a = 0, b = 1;
  let temp;
  while (true) {
    temp = a + b;
    yield temp;
    a = b;
    b = temp;
  }
}

export function compute(limit: number): string {
  const fib_gen = fibGen();
  let result = 0;

  for (const n of fib_gen) {
    if (n >= limit) {
      break;
    }
    if (n % 2 === 0) {
      result += n;
    }
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
