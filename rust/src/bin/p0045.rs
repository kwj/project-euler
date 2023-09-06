// Project Euler: Problem 45

/*
  Hexagonal numbers are also triangle numbers.
  Therefore, we search for hexagonal numbers which are also pentagonal numbers.

  P{i} = H{j}
  i(3i - 1) / 2 = j(2j - 1)
  3i^2 - i = 4j^2 - 2j
  (6i - 1)^2 - 1 = 3(4j - 1)^2 - 3
  -->
  (6i - 1)^2 - 3(4j - 1)^2 = -2
   ------        ------
      X            Y

  see https://imomath.com/index.cgi?page=ntPellsEquationPellType
  -->
  z0 = 2 + sqrt(3), z = 1 + sqrt(3)
  X{n} + Y{n} sqrt(3) = (1 + sqrt(3)) (2 + sqrt(3))^n

  X{n} = 2X{n-1} + 3Y{n-1}
  Y{n} = X{n-1} + 2Y{n-1}
    where X{0} = 1, Y{0} = 1
*/

euler::run_solver!(45);

fn solve() -> String {
    compute(3).to_string()
}

fn compute(mut nth: i64) -> i64 {
    let mut x: i64 = 1;
    let mut y: i64 = 1;

    loop {
        (x, y) = (2 * x + 3 * y, x + 2 * y);
        if x % 6 == 5 && y % 4 == 3 {
            nth -= 1;
            if nth == 0 {
                break;
            }
        }
    }
    let j = (y + 1) / 4;
    j * (2 * j - 1)
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0045_1st() {
        assert_eq!(compute(1), 1);
    }

    #[test]
    fn p0045_2nd() {
        assert_eq!(compute(2), 40755);
    }

    #[test]
    fn p0045_3rd() {
        assert_eq!(compute(3), 1533776805);
    }
}
