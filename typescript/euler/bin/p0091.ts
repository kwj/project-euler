// project euler: problem 91

/*
  0 <= x1,x2,y1,y2 <= 50
  P(x1,y1), Q(x2,y2)

  case #1:
    1-1) the right angle is O(0,0)
      P(x1>0,0) and Q(0,y2>0)
      --> 50 * 50
    1-2) the right angle is on the x-axis [P(x1>0,y1=0)]
      P(x1>0,0) and Q(x2=x1,y2>0)
      --> 50 * 50
    1-3) the right angle is on the y-axis [Q(x1=0,y2>0)]
      P(x1>0,y1=y2) and Q(0,y2>0)
      --> 50 * 50

  case #2:
    2-1) P(x1,y1) is the right angle and Qi(x_i>x1,y_i<y1)
      Y
       | P(x1,y1)
       |   #   Q1(a1,b1)
       |  .       *   Q2(a2,b2)
       | .               *
       |.                       *
      -O------------------------------------- X
                                            *

      --> min((y1 / (x / gcd(x1,y1))), ((50-x1) / (y / gcd(x1,y1))))

    2-2) P(x1,y1) is the right angle and Q(x2<x1,y2>y1)
      same qty as case 2-1.  [mirror on the y=x linne]
*/

import { gcd } from "../lib/math.ts";
import { range } from "../lib/util.ts";

export const compute = (xSize: number, ySize: number): string => {
  const case2 = (): number => {
    const trunc = Math.trunc;
    let acc = 0;
    for (const x of range(1, xSize + 1)) {
      for (const y of range(1, ySize + 1)) {
        acc += Math.min(
          trunc(y * gcd(x, y) / x),
          trunc((xSize - x) * gcd(x, y) / y),
        );
      }
    }

    return acc * 2;
  };

  return String((xSize * ySize) * 3 + case2());
};

export const solve = (): string => compute(50, 50);
