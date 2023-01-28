
// project euler: problem 81

import { runningReduce } from "std/collections/running_reduce.ts";
import { assetData } from "../lib/asset.ts";
import { range } from "../lib/util.ts";

export function compute(fn: (...valus: number[]) => number, data: string): string {
  function parseData(data: string): number[][] {
    function splitLines(str: string): string[] {
      const result = str.split(/\r?\n/);
      if (result.at(-1) === "") {
        return result.slice(0, -1);
      } else {
        return result;
      }
    }

    return splitLines(data).map((x) => x.split(",").map((y) => Number(y)));
  }

  const matrix = parseData(data);

  let prev = [Number.MAX_SAFE_INTEGER].concat(runningReduce(matrix[0], (acc, cur) => acc + cur, 0));
  for (const work of matrix.slice(1)) {
    work.unshift(Number.MAX_SAFE_INTEGER);
    for (const i of range(1, work.length)) {
      work[i] += fn(work[i - 1], prev[i]);
    }
    prev = work;
  }

  return String(prev.at(-1));
}

export function solve(): void {
  try {
    const data = new TextDecoder().decode(assetData("p081_matrix.txt"));

    const t0 = performance.now();
    const result = compute(Math.min, data);
    const t1 = performance.now();
    const duration_ms = (t1 - t0).toFixed(4);

    console.log(`Answer: ${result}`);
    console.log(`Elapsed time: ${duration_ms} msec.`);
  } catch (err) {
    console.error(err.message);
  }

  return;
}
