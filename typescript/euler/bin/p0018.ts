
// project euler: problem 18

import { zip } from "std/collections/zip.ts";

const data = [
  [75],
  [95, 64],
  [17, 47, 82],
  [18, 35, 87, 10],
  [20,  4, 82, 47, 65],
  [19,  1, 23, 75,  3, 34],
  [88,  2, 77, 73,  7, 63, 67],
  [99, 65,  4, 28,  6, 16, 70, 92],
  [41, 41, 26, 56, 83, 40, 80, 70, 33],
  [41, 48, 72, 33, 47, 32, 37, 16, 94, 29],
  [53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14],
  [70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57],
  [91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48],
  [63, 66,  4, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31],
  [ 4, 62, 98, 27, 23,  9, 70, 98, 73, 93, 38, 53, 60,  4, 23],
];

function selectLeaf(fn: (...valus: number[]) => number, lst: number[]): number[] {
  const result = [];
  let prev = lst[0];
  for (const i of lst) {
    result.push(fn(prev, i));
    prev = i;
  }

  return result.slice(1);
}

export function compute(fn: (...valus: number[]) => number, nums: number[][]): string {
  nums.reverse();
  let prev = new Array(nums[0].length + 1);
  prev.fill(0);

  for (const lst of nums) {
    prev = zip(lst, selectLeaf(fn, prev)).map((tpl) => tpl[0] + tpl[1]);
  }

  return String(prev[0]);
}

export function solve(): void {
  const t0 = performance.now();
  const result = compute(Math.max, data);
  const t1 = performance.now();
  const duration_ms = (t1 - t0).toFixed(4);

  console.log(`Answer: ${result}`);
  console.log(`Elapsed time: ${duration_ms} msec.`);

  return;
}
