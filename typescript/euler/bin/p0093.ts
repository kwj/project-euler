// project euler: problem 93

/*
combination of numberes:
  nCk = C(n,k) = C(9,4) = 9*8*7*6 / 4*3*2*1 = 126

arithmetic operations (fourOps):
  commutative:
    addition: X + Y
    multiplication: X * Y
  no-commutative:
    subtraction:  X - Y, Y - X
    division: X / Y, Y / X

patterns:
  A, B, C, D: numbers
    4! = 24

  [1] ((A op B) op C) op D
       ^^^--^^^
      ^^^^^^^^^^--^^^
      ^^^^^^^^^^^^^^^^--^^
  [2] (A op B) op (C op D)
      ^^^--^^^    ^^^--^^^
      ^^^^^^^^^--^^^^^^^^^

  ^-^: We can ignore the order of two terms because
       four_ops() considers no-commutative operations.
*/

import { cartesianProduct, combinations } from "combinatorics/mod.ts";
import { Rational, rational } from "../lib/rational.ts";

const four_ops = (x1: Rational, x2: Rational): Rational[] => {
  const result: Rational[] = [x1.add(x2), x1.mul(x2), x1.sub(x2), x2.sub(x1)];
  if (x1.num !== 0n) {
    result.push(x2.div(x1));
  }
  if (x2.num !== 0n) {
    result.push(x1.div(x2));
  }

  return result;
};

const case_1 = (x1: Rational, x2: Rational, rest: Rational[]): Rational[] => {
  // ((A op B) op C) op D
  let result: Rational[] = [];
  for (const ab of four_ops(x1, x2)) {
    // C: rest[0], D: rest[1]
    for (const abc of four_ops(ab, rest[0])) {
      result = result.concat(four_ops(abc, rest[1]));
    }
    // C: rest[1], D: rest[0]
    for (const abc of four_ops(ab, rest[1])) {
      result = result.concat(four_ops(abc, rest[0]));
    }
  }

  return result;
};

const case_2 = (x1: Rational, x2: Rational, rest: Rational[]): Rational[] => {
  // (A op B) op (C op D)
  let result: Rational[] = [];
  const ab_lst = four_ops(x1, x2);
  const cd_lst = four_ops(rest[0], rest[1]);

  for (const [ab, cd] of cartesianProduct(ab_lst, cd_lst)) {
    result = result.concat(four_ops(ab, cd));
  }

  return result;
};

const make_numbers = (lst: Rational[]): Set<number> => {
  const result = new Set<number>();
  for (let i = 0; i < lst.length; i++) {
    for (let j = i + 1; j < lst.length; j++) {
      const rest: Rational[] = [];
      for (let k = 0; k < lst.length; k++) {
        if (k !== i && k !== j) {
          rest.push(lst[k]);
        }
      }

      for (const rat of case_1(lst[i], lst[j], rest)) {
        if (rat.isInteger() === true) {
          result.add(Number(rat.num));
        }
      }
      for (const rat of case_2(lst[i], lst[j], rest)) {
        if (rat.isInteger() === true) {
          result.add(Number(rat.num));
        }
      }
    }
  }

  return result;
};

const count_consec_numbers = (lst: Rational[]): number => {
  const n_set = make_numbers(lst);
  let cnt = 1;
  while (n_set.has(cnt)) {
    cnt += 1;
  }

  return cnt - 1;
};

export const compute = (): string => {
  let max_count = 0;
  let nums: Rational[] = [];
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
