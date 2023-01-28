
// project euler: problem 62

export function compute(numOfPerms: number): string {
  function makeKey(n: number): string {
    return String(n).split("").sort().join("");
  }

  const tbl = new Map<string, number[]>();
  let n = 0;
  while (true) {
    n += 1;
    const key = makeKey(n * n * n);
    if (tbl.has(key) === true) {
      tbl.set(key, tbl.get(key)!.concat(n));
      if (tbl.get(key)!.length === numOfPerms) {
        return String(tbl.get(key)![0] ** 3);
      }
    } else {
      tbl.set(key, [n]);
    }
  }
}

export function solve(): void {
  const t0 = performance.now();
  const result = compute(5);
  const t1 = performance.now();
  const duration_ms = (t1 - t0).toFixed(4);

  console.log(`Answer: ${result}`);
  console.log(`Elapsed time: ${duration_ms} msec.`);

  return;
}
