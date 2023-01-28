
// project euler: problem 71

/*
  Farey sequence

  2/5, 3/7
    -> 2/5, (2+3)/(5+7), 3/7
    -> 2/5, (2+3)/(5+7), (2+3+3)/(5+7+7), 3/7
    -> 2/5, (2+3)/(5+7), (2+3+3)/(5+7+7), (2+3+3+3)/(5+7+7+7), 3/7
     ...
    -> 2/5, ..., (2+3x)/(5+7x), 3/7

      5+7x <= 1_000_000
*/

export function compute(limit: number): string {
  return String(2 + 3 * Math.trunc((limit - 5) / 7));
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
