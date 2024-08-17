// project euler: problem 68

/*
  Note: This problem can be solved by pen and paper.
*/

import { range } from "../lib/util.ts";

const allRings = (nGon: number): string[] => {
  // weight: sum of each node on line
  //   minimum weight: (n_gon * 2) + 1 + 2 = n_gon * 2 + 3
  //   maximum weight: 1 + (n_gon * 2 - 1) + (n_gon * 2) = n_gon * 4
  let result: string[] = [];
  for (const weight of range(nGon * 2 + 3, nGon * 4 + 1)) {
    result = result.concat(findRings(nGon, weight));
  }

  return result;
};

const deleteElmnt = <T>(elm: T, arr: T[]): T[] => arr.filter((x) => x !== elm);

const findRings = (nGon: number, weight: number): string[] => {
  const numbers = range(1, nGon * 2 + 1);
  const rings: string[] = []; // rings found

  const makeStr = (ring: number[]): string => {
    let result = "";
    for (let i = 0; i < nGon * 2; i += 2) {
      result = `${result}${ring[i + 1]}${ring[i]}${ring[i + 2]}`;
    }

    return result;
  };

  // ring :: number[]
  //   +-+-+--     ---+-+-+   X: first selected inner node -- ring[0]
  //   |X|Y|   ...    | |Z|   Y: first selected outer node -- ring[1]
  //   +-+-+--     ---+-+-+   Z: last selected inner node -- ring.at(-1)
  //    0
  //
  //     [Y]
  //       \
  //        [X]   *
  //       /   \ /
  //     ??     *
  //    / \    /
  //  ??  [Z]-*-- *
  //        \
  //         ??
  const nextStates = (
    [ring, rest]: [number[], number[]],
  ): [number[], number[]][] => {
    const states: [number[], number[]][] = [];

    if (rest.length === 1) {
      const n = rest[0];
      if (n > ring[1] && n + ring[0] + ring.at(-1)! === weight) {
        // a magic n-gon ring found
        rings.push(makeStr([...ring, n, ring[0]]));
      }
    } else {
      for (const outer of rest) {
        if (
          ring.length === 1 && outer > nGon + 1 ||
          ring.length > 1 && outer < ring[1]
        ) {
          continue;
        }
        const inner = weight - outer - ring.at(-1)!;
        if (outer === inner) {
          continue;
        }
        if (!rest.includes(inner)) {
          continue;
        }
        states.push([
          [...ring, outer, inner],
          deleteElmnt(inner, deleteElmnt(outer, rest)),
        ]);
      }
    }

    return states;
  };

  let queue: [number[], number[]][] = [];
  for (const x of numbers) {
    queue.push([[x], deleteElmnt(x, numbers)]);
  }

  while (queue.length > 0) {
    const state = queue.pop()!;
    queue = queue.concat(nextStates(state));
  }

  return rings;
};

export const compute = (nGon: number): string => {
  let result = allRings(nGon).toSorted();
  if (nGon === 5) {
    result = result.filter((x) => x.length === 16);
  }

  return result.at(-1)!;
};

export const solve = (): string => compute(5);
