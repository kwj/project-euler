// project euler: problem 93

/*
combination of numberes:
  nCk = C(n,k) = C(9,4) = 9*8*7*6 / 4*3*2*1 = 126

arithmetic operations (four_ops):
  commutative:
    addition: X + Y
    multiplication: X * Y
  no-commutative:
    subtraction:  X - Y, Y - X
    division: X / Y, Y / X

patterns:
  d1, d2, d3, d4: numbers

  [case 1]
     ((d1 OP d2) OP d3) OP d4
      ^^^----^^^
     ^^^^^^^^^^^----^^^
     ^^^^^^^^^^^^^^^^^^----^^

     ((d1 OP d2) OP d4) OP d3
      ^^^----^^^
     ^^^^^^^^^^^----^^^
     ^^^^^^^^^^^^^^^^^^----^^
  [case 2]
     (d1 OP d2) OP (d3 OP d4)
     ^^^----^^^    ^^^----^^^
     ^^^^^^^^^^----^^^^^^^^^^

  ^^-^^: We can ignore the order of the two terms because
         four_ops() considers no-commutative operations.
*/

import { cartesianProduct, combinations } from "combinatorics/mod.ts";
import { Ratio, rational } from "../lib/rational.ts";

const four_ops = (x1: Ratio, x2: Ratio): Ratio[] => {
  if (x1.isZero()) {
    return [x2.negate(), rational(0), x2];
  }

  if (x2.isZero()) {
    return [x1.negate(), rational(0), x1];
  }

  return [
    x1.add(x2),
    x1.mul(x2),
    x1.sub(x2),
    x2.sub(x1),
    x1.div(x2),
    x2.div(x1),
  ];
};

const case_1 = (x1: Ratio, x2: Ratio, rest: Ratio[]): Ratio[] => {
  let result: Ratio[] = [];
  for (const d1d2 of four_ops(x1, x2)) {
    // d3: rest[0], d4: rest[1]
    for (const d1d2d3 of four_ops(d1d2, rest[0])) {
      result = result.concat(four_ops(d1d2d3, rest[1]));
    }
    for (const d1d2d4 of four_ops(d1d2, rest[1])) {
      result = result.concat(four_ops(d1d2d4, rest[0]));
    }
  }

  return result;
};

const case_2 = (x1: Ratio, x2: Ratio, rest: Ratio[]): Ratio[] => {
  let result: Ratio[] = [];
  for (
    const [d1d2, d3d4] of cartesianProduct(
      four_ops(x1, x2),
      four_ops(rest[0], rest[1]),
    )
  ) {
    result = result.concat(four_ops(d1d2, d3d4));
  }

  return result;
};

const make_numbers = (lst: Ratio[]): Set<number> => {
  // The parameter `lst` must be a Ratio list of four elements.
  const result = new Set<number>();
  for (let i = 0; i < lst.length - 1; i++) {
    for (let j = i + 1; j < lst.length; j++) {
      const rest: Ratio[] = [];
      for (let k = 0; k < lst.length; k++) {
        if (k !== i && k !== j) {
          rest.push(lst[k]);
        }
      }

      // [1] ((d1 OP d2) OP d3) OP d4
      //     ((d1 OP d2) OP d4) OP d3
      for (const rat of case_1(lst[i], lst[j], rest)) {
        if (rat.isInteger()) {
          result.add(Number(rat.num));
        }
      }

      // [2] (d1 OP d2) OP (d3 OP d4)
      if (i !== 0) {
        continue;
      }
      for (const rat of case_2(lst[i], lst[j], rest)) {
        if (rat.isInteger()) {
          result.add(Number(rat.num));
        }
      }
    }
  }

  return result;
};

const count_consec_numbers = (lst: Ratio[]): number => {
  const n_set = make_numbers(lst);
  let cnt = 1;
  while (n_set.has(cnt)) {
    cnt += 1;
  }

  return cnt - 1;
};

export const compute = (): string => {
  let max_count = 0;
  let nums: Ratio[] = [];
  const rational_numbers = [
    rational(1),
    rational(2),
    rational(3),
    rational(4),
    rational(5),
    rational(6),
    rational(7),
    rational(8),
    rational(9),
  ];

  for (const lst of combinations(rational_numbers, 4)) {
    const len = count_consec_numbers(lst);
    if (len > max_count) {
      max_count = len;
      nums = lst;
    }
  }

  let result = 0;
  for (const x of nums) {
    result = result * 10 + Number(x.num);
  }

  return String(result);
};

export const solve = (): string => compute();
