// project euler: problem 14

import { max } from "../lib/math.ts";
import { range } from "../lib/util.ts";

export const compute = (limit: number): string => {
  const cache = new Array(limit);
  cache.fill(0);
  cache[1] = 1;

  for (let cur of range(Math.trunc(limit / 2), limit)) {
    if (cache[cur] !== 0) {
      continue;
    }

    const path: number[] = [];
    while (cur >= limit || cache[cur] === 0) {
      path.push(cur);
      if (cur % 2 === 0) {
        cur = Math.trunc(cur / 2);
      } else {
        cur = 3 * cur + 1;
      }
    }

    let steps = cache[cur] + 1;
    path.reverse();
    for (const i of path) {
      if (i < limit) {
        cache[i] = steps;
      }
      steps += 1;
    }
  }

  return String(cache.indexOf(max(cache)));
};

export const solve = (): string => compute(1_000_000);
