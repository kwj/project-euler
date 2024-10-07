// Project Euler: Problem 71

/*
  Farey sequence

  2/5, 3/7
    -> 2/5, (2+3)/(5+7), 3/7
    -> 2/5, (2+3)/(5+7), (2+3+3)/(5+7+7), 3/7
    -> 2/5, (2+3)/(5+7), (2+3+3)/(5+7+7), (2+3+3+3)/(5+7+7+7), 3/7
     ...
    -> 2/5, ..., (2+3x)/(5+7x), 3/7

      5+7x <= 1_000_000
*/

euler::run_solver!(71);

fn solve() -> String {
    compute(1_000_000).to_string()
}

fn compute(limit: i64) -> i64 {
    debug_assert!(limit >= 8);

    2 + 3 * ((limit - 5) / 7)
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0071_8() {
        assert_eq!(compute(8), 2);
    }
    #[test]
    fn p0071_1000000() {
        assert_eq!(compute(1_000_000), 428570);
    }
}
