
// project euler: problem 19

import { runningReduce } from "std/collections/running_reduce.ts";
import { range } from "../lib/util.ts";

export function compute(): string {
  const commonYear = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
  const leapYear = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];

  // days per month (Jan 1901 - *Nov* 2000)
  const _temp = commonYear.concat(commonYear, commonYear, leapYear);
  let days = _temp;
  for (const _ of range(1, 25)) {
    days = days.concat(_temp);
  }
  days = days.slice(0, -1);

  // Jan 1, 1900 was Monday and assume this day is the first day. (Monday is '1 mod 7 = 1')
  // And then, the year 1900 was common year = 365 days.
  // --> Jan 1, 1901 was Tuesday since (1 + 365) mod 7 = 2.
  //     Feb 1, 1901 was Firday since ((1 + 365) + 31) mod 7 = 5.
  //     ... and so on
  return String(runningReduce([1 + 365].concat(days), (sum, current) => sum + current, 0).filter((x) => x % 7 === 0).length);
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
