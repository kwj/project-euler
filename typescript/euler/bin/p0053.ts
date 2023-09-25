// project euler: problem 53

export function compute(num: number, boundary: number): string {
  let n = num, x = num;
  let c = 1, r = 1;
  let answer = boundary > 0 ? 0 : num * 2;

  while (r <= (n >> 1)) {
    c = c * x / r;
    if (c > boundary) {
      answer += n - (r * 2) + 1;
      c = c * r / n;
      n -= 1;
    } else {
      r += 1;
    }
    x -= 1;
  }

  return String(answer);
}

export function solve(): void {
  const t0 = performance.now();
  const result = compute(100, 1_000_000);
  const t1 = performance.now();
  const duration_ms = (t1 - t0).toFixed(4);

  console.log(`Answer: ${result}`);
  console.log(`Elapsed time: ${duration_ms} msec.`);

  return;
}
