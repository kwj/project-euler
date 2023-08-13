
// project euler: problem 63

/*
  n - 1 <= log10(m^n) < n    [m>0, n>0]
    --> n - 1 <= n * log10(m) < n
    --> m < 10
   and
    --> (n - 1)/n <= log10(m)
    --> 10 ^ (n - 1)/n <= 10 ^ log10(m)
    --> log10(10 ^ (n - 1)/n) <= log10(m)
    --> 10 ^ (n - 1)/n <= m
*/

export function compute(): string {
  let acc = 0;
  let cnt = 0;
  let m = 1;
  let n = 1;
  while (m < 10) {
    while (Math.pow(10, ((n - 1) / n)) <= m) {
      n += 1;
      cnt += 1;
    }
    m += 1;
    acc += cnt;
  }

  return String(acc);
}

export function solve(): void {
  const t0 = performance.now();
  const result = compute();
  const t1 = performance.now();
  const duration_ms = (t1 - t0).toFixed(4);

  console.log(`Answer: ${result}`);
  console.log(`Elapsed time: ${duration_ms} msec.`);

  return;
}
