// project euler: problem 48

export function compute(exp: bigint): string {
  const modulus = 10n ** 10n;

  let acc = 0n;
  for (let x = 1n; x <= exp; x++) {
    if (x % 10n !== 0n) {
      acc += (x ** x) % modulus;
    }
  }
  acc = acc % modulus;

  return acc.toString().padStart(10, "0");
}

export function solve(): void {
  const t0 = performance.now();
  const result = compute(1_000n);
  const t1 = performance.now();
  const duration_ms = (t1 - t0).toFixed(4);

  console.log(`Answer: ${result}`);
  console.log(`Elapsed time: ${duration_ms} msec.`);

  return;
}
