// project euler: problem 1

function sumMultiple(n: number, limit: number): number {
  const trunc = Math.trunc;
  const upper = limit - 1;

  return trunc((n + (upper - (upper % n))) * trunc(upper / n) / 2);
}

export function compute(limit: number): string {
  return String(
    sumMultiple(3, limit) + sumMultiple(5, limit) - sumMultiple(15, limit),
  );
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
