// project euler: problem 43

import { sum } from "../lib/math.ts";
import { range } from "../lib/util.ts";

export function compute(): string {
  let lst = [""];
  const charLst = range("0".charCodeAt(0), "9".charCodeAt(0) + 1).map((x) =>
    String.fromCharCode(x)
  );

  for (const d of [1, 1, 17, 13, 11, 7, 5, 3, 2, 1]) {
    const next_lst: string[] = [];
    for (const s of lst) {
      for (const x of charLst) {
        if (s.includes(x) === false && Number((x + s).slice(0, 3)) % d === 0) {
          next_lst.push(x + s);
        }
      }
    }
    lst = next_lst;
  }

  return String(sum(lst.map((x) => Number(x))));
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
