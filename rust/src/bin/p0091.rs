// Project Euler: Problem 91

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
      -O---------------------------------------- X
                                              *

      --> min((y1 / (x / gcd(x1,y1))), ((50-x1) / (y / gcd(x1,y1))))

    2-2) P(x1,y1) is the right angle and Q(x2<x1,y2>y1)
      same qty as case 2-1.  [mirror on the y=x line]
*/

euler::run_solver!(91);

fn solve() -> String {
    compute(50, 50).to_string()
}

fn compute(x_size: i64, y_size: i64) -> i64 {
    use euler::math;
    use std::cmp;

    debug_assert!(x_size > 0 && y_size > 0);

    fn case_1(x_upper: i64, y_upper: i64) -> i64 {
        x_upper * y_upper * 3
    }

    fn case_2(x_upper: i64, y_upper: i64) -> i64 {
        let mut acc: i64 = 0;

        for (x, y) in itertools::iproduct!(1..=x_upper, 1..=y_upper) {
            acc += cmp::min(y * math::gcd(x, y) / x, (x_upper - x) * math::gcd(x, y) / y);
        }

        acc * 2
    }

    case_1(x_size, y_size) + case_2(x_size, y_size)
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0091_2() {
        assert_eq!(compute(2, 2), 14);
    }

    #[test]
    fn p0091_50() {
        assert_eq!(compute(50, 50), 14234);
    }
}
