// project euler: problem 93

/*
It is slow, but I use Reverse Polish notation and Python's fraction module.

  Infix notation and Reverse Polish notation

    case 1:
       IN: ((x1 OP1 x2) OP2 x3) OP3 x4
      RPN: x1 x2 OP1 x3 OP2 x4 OP3

    # The case 2 can be ignored since it is covered on case 1.
    case 2:
       IN: (x1 OP1 (x2 OP2 x3)) OP3 x4
      RPN: x1 x2 x3 OP2 OP1 x4 OP3

    case 3:
       IN: (x1 OP1 x2) OP2 (x3 OP3 x4)
      RPN: x1 x2 OP1 x3 x4 OP3 OP2

    # The following cases can be ignored since the following reasons
    #   - they are symmetrical with the above cases
    #   - numbers(x{n}) are provided as permutations
    #   - operators(OP{n}) are provided as permutations with repetition

    case 4:   (it is symmetrical with case 2)
       IN: x1 OP1 ((x2 OP2 x3) OP3 x4)
      RPN: x1 x2 x3 OP2 x4 OP3 OP1

    case 5:   (it is symmetrical with case 1)
       IN: x1 OP1 (x2 OP2 (x3 OP3 x4))
      RPN: x1 x2 x3 x4 OP3 OP2 OP1
*/

import {
  permutations,
  permutationsWithReplacement,
} from "combinatorics/mod.ts";
import { Rational, rational } from "../lib/rational.ts";
import { dedupSort } from "../lib/util.ts";

type RPNelm = string | Rational;

const calcRPN = (rpn: RPNelm[]): Rational => {
  const stack: Rational[] = [];
  for (const elm of rpn) {
    if (typeof elm !== "string") {
      stack.push(elm);
    } else {
      const e2 = stack.pop()!;
      const e1 = stack.pop()!;
      switch (elm) {
        case "+":
          stack.push(e1.add(e2));
          break;
        case "-":
          stack.push(e1.sub(e2));
          break;
        case "*":
          stack.push(e1.mul(e2));
          break;
        case "/":
          // Skip when zero divide will occur and return 0.
          // We ignore numbers less than one, later.
          if (e2.num === 0n) {
            return rational(0, 1);
          }
          stack.push(e1.div(e2));
          break;
      }
    }
  }

  return stack[0];
};

const numsGenerator = function* (): Generator<Rational[], void, unknown> {
  for (let a = 0; a < 10; a++) {
    for (let b = a + 1; b < 10; b++) {
      for (let c = b + 1; c < 10; c++) {
        for (let d = c + 1; d < 10; d++) {
          yield [rational(a), rational(b), rational(c), rational(d)];
        }
      }
    }
  }
};

const getMaxConsecNum = (x: Rational[], opSets: string[][]): number => {
  let acc: Rational[] = [];
  for (const [x1, x2, x3, x4] of permutations(x)) {
    for (const [op1, op2, op3] of opSets) {
      acc = acc.concat(
        calcRPN([x1, x2, op1, x3, op2, x4, op3]),
        calcRPN([x1, x2, op1, x3, x4, op3, op2]),
      );
    }
  }
  const nationalNumbers = acc.filter((x) =>
    x!.isInteger() === true && x!.num > 0n
  ) as Rational[];
  const result = dedupSort(
    nationalNumbers.map((x) => Number(String(x.num))),
    (a, b) => a - b,
  );

  let cnt: number;
  for (cnt = 0; cnt === result[cnt] - 1; cnt++);

  return cnt;
};

export const compute = (): string => {
  const opSets: string[][] = [
    ...permutationsWithReplacement(["+", "-", "*", "/"], 3),
  ];
  const nums_gen = numsGenerator();

  let answer: [number, Rational[]] = [0, []];
  for (const x of nums_gen) {
    const result = getMaxConsecNum(x, opSets);
    //console.log(result);
    if (result > answer[0]) {
      answer = [result, x];
    }
  }

  return answer[1].map((x) => String(x.num)).join("");
};

export const solve = (): string => compute();
