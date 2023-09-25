# project euler: problem 91

#   0 <= x1,x2,y1,y2 <= 50
#   P(x1,y1), Q(x2,y2)
#
#   case #1:
#     1-1) the right angle is O(0,0)
#       P(x1>0,0) and Q(0,y2>0)
#       --> 50 * 50
#     1-2) the right angle is on the x-axis [P(x1>0,y1=0)]
#       P(x1>0,0) and Q(x2=x1,y2>0)
#       --> 50 * 50
#     1-3) the right angle is on the y-axis [Q(x1=0,y2>0)]
#       P(x1>0,y1=y2) and Q(0,y2>0)
#       --> 50 * 50
#
#   case #2:
#     2-1) P(x1,y1) is the right angle and Qi(x_i>x1,y_i<y1)
#       Y
#        | P(x1,y1)
#        |   #   Q1(a1,b1)
#        |  .       *   Q2(a2,b2)
#        | .               *
#        |.                       *
#       -O------------------------------------- X
#                                             *
#
#       --> min((y1 / (x / gcd(x1,y1))), ((50-x1) / (y / gcd(x1,y1))))
#
#     2-2) P(x1,y1) is the right angle and Q(x2<x1,y2>y1)
#       same qty as case 2-1.  [mirror on the y=x linne]

from math import gcd


def compute(x_size: int, y_size: int) -> str:
    def case_2() -> int:
        return 2 * sum(
            min(y * gcd(x, y) // x, (x_size - x) * gcd(x, y) // y)
            for x in range(1, x_size + 1)
            for y in range(1, y_size + 1)
        )

    return str((x_size * y_size) * 3 + case_2())


def solve() -> str:
    return compute(50, 50)
