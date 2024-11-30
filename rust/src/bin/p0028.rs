// Project Euler: Problem 28

/*
  21 22 23 24 25
  20  7  8  9 10
  19  6  1  2 11
  18  5  4  3 12
  17 16 15 14 13
         |  |  |  |
      (n=0, 1, 2, 3, ...)

  the upper right number is:
    1    [n=0]
    (2n+1)**2    [n=>1]

  so, the sum of numbers in the four corners is:
    (2n+1)**2 + ((2n+1)**2 - 2n) + ((2n+1)**2 - 4n) + ((2n+1)**2 - 6n)
      = 16n**2 + 4n + 4   [n>=1]

  Answer: 1 + sum_{n=1}^{(1001-1)/2} (16n**2 + 4n + 4)
*/

euler::run_solver!(28);

fn solve() -> String {
    compute(1_001).to_string()
}

fn compute(length: i64) -> i64 {
    fn f(x: i64) -> i64 {
        16 * x * x + 4 * x + 4
    }

    debug_assert!(length.is_positive() && length % 2 == 1);

    1 + (1..=((length - 1) / 2)).map(f).sum::<i64>()
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0028_length_5() {
        assert_eq!(compute(5), 101);
    }
    #[test]
    fn p0028_length_1001() {
        assert_eq!(compute(1_001), 669171001);
    }
}
