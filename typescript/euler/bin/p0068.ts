// project euler: problem 68

/*
  This problem can be solved with pen and paper.
  No computer is needed.

  1) maximam 16-digit string
    number '10' must be at outer position.

  2) If the condition is met when the numbers '6' to '10' are at
     outer positions, one of them is the answer.

  --> Assume that numbers '6' to '10' are at the outer positions.

  3) the number '1' to '5' are on the pentagon.

  4) the sum of all lines is (1+2+3+4+5)*2 + (6+7+8+9+10) = 70.

  5) the sum of numbers on each lines are all 14.

  6) number '1' and '3' must be on the line which contains '10'.

    case #1       case #2
        10            10
          \             \
           3  8,9        1  6,7,8,9
            \ /           \ /
             1             3

  7) case #1-1:
          10
            \
             3   8
              \ /
               1
              /
             5

       <a>                 <b>
          10              10
            \               \
             3   8           3   8
           /  \ /          /  \ /
          4    1   [NG]   2    1    [NG]
         /\   /           \   /
       [7] 2-5--7          4-5-[5]
            \
             8

     case #1-2:
          10
            \
             3   9
              \ /
               1
              /
             4

       <a>                 <b>
          10                10
            \                 \
             3   9             3   9
              \ /            /  \ /
               1   [NG]     5    1   [OK] 6,5,3;10,3,1;9,1,4;8,4,2;7,2,5
              /            /\   /
           5-4-[5]        6  2-4--8
                              \
                               7



        <e1>
          a0
            \   <e2>
            a1  a3
           /  \ /
         a8   a2
         /\   /
       a9 a6-a4-a5 <e3>
     <e5>   \
             a7
             <e4>

  8) case #2-1:
          10
            \
             1   9
              \ /
               3
              /
             2
       <a>                 <b>
          10                   10
            \                    \
             1   9                1   9
           /  \ /               /  \ /
          5    3   [NG]        4    3  [NG]
          \   /                \   /
           4-2                  5-2
            \                    \
            [5]                  [5]

     case #2-2:
          10
            \
             1   8
              \ /
               3   [NG]
              /
            [3]

     case #2-3:
          10
            \
             1   7
              \ /
               3
              /
             4

       <a>                 <b>
          10                   10
            \                    \
             1   7                1   7
           /  \ /               /  \ /
          5    3   [NG]        2    3  [NG]
          \   /                \   /
           2-4                  5-4
            \                    \
            [7]                  [7]

     case #2-4:
          10
            \
             1   6
              \ /
               3
              /
             5

       <a>                 <b>
          10                   10
            \                    \
             1   6                1   6
           /  \ /               /  \ /
          2    3   [NG]        4    3   [OK] 6,3,5;7,5,2;8,2,4;9,4,1;10,1;3
          \   /               /\   /
           4-5-[5]           9  2-5-7
                                 \
                                  8

  9) There are two patterns which satisfy the condition.
     The answer is case #1-2 <b>.
      case #1-2 <b>
        6,5,3;10,3,1;9,1,4;8,4,2;7,2,5
      case #2-4 <b>
        6,3,5;7,5,2;8,2,4;9,4,1;10,1;3

  ----
  I'll use brute force with the following conditions.

  list [a0, a1, a2, a3, a4, a5, a6, a7, a8, a9]
   - one of {a3, a5, a7, a9} is 10
   - a0 < {a3, a5, a7, a9}
   - each lines (e1..e5) have same weight
*/

import { permutations } from "combinatorics/mod.ts";
import { sum } from "../lib/math.ts";
import { range } from "../lib/util.ts";

export const compute = (): string => {
  const result: string[] = [];
  for (
    const [a0, a1, a2, a3, a4, a5, a6, a7, a8, a9] of permutations(
      range(1, 10 + 1),
    )
  ) {
    if (a0 > a3 || a0 > a5 || a0 > a7 || a0 > a9) {
      continue;
    }
    if ([a3, a5, a7, a9].includes(10) === false) {
      continue;
    }

    // deno-fmt-ignore
    const ring = [[a0, a1, a2], [a3, a2, a4], [a5, a4, a6], [a7, a6, a8], [a9, a8, a1]];
    const edgeWeights = ring.map((x) => sum(x));
    if (edgeWeights.every((x) => x === edgeWeights[0]) === true) {
      result.push(ring.map((x) => x.map((y) => String(y)).join("")).join(""));
    }
  }

  return result.reverse()[0];
};

export const solve = (): string => compute();
