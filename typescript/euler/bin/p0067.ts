// project euler: problem 67

import { zip } from "std/collections/zip.ts";
import { assetData } from "../lib/asset.ts";

function selectLeaf(
  fn: (...valus: number[]) => number,
  lst: number[],
): number[] {
  const result = [];
  let prev = lst[0];
  for (const i of lst) {
    result.push(fn(prev, i));
    prev = i;
  }

  return result.slice(1);
}

export function compute(
  fn: (...valus: number[]) => number,
  data: string,
): string {
  function parseData(data: string): number[][] {
    function splitLines(str: string): string[] {
      const result = str.split(/\r?\n/);
      if (result.at(-1) === "") {
        return result.slice(0, -1);
      } else {
        return result;
      }
    }

    return splitLines(data).map((x) => x.split(" ").map((y) => Number(y)));
  }

  const nums = parseData(data);
  nums.reverse();
  let prev = nums[0];
  for (const lst of nums.slice(1)) {
    prev = zip(lst, selectLeaf(fn, prev)).map((tpl) => tpl[0] + tpl[1]);
  }

  return String(prev[0]);
}

export function solve(): void {
  try {
    const data = new TextDecoder().decode(assetData("p067_triangle.txt"));

    const t0 = performance.now();
    const result = compute(Math.max, data);
    const t1 = performance.now();
    const duration_ms = (t1 - t0).toFixed(4);

    console.log(`Answer: ${result}`);
    console.log(`Elapsed time: ${duration_ms} msec.`);
  } catch (err) {
    console.error(err.message);
  }

  return;
}
