// Project Euler: Problem 6

euler::run_solver!(6);

fn solve() -> String {
    compute(100).to_string()
}

fn compute(upper: i64) -> i64 {
    debug_assert!(upper > 0);

    (sum_of_squares(upper) - square_of_sum(upper)).abs()
}

fn sum_of_squares(n: i64) -> i64 {
    n * (n + 1) * (2 * n + 1) / 6
}

fn square_of_sum(n: i64) -> i64 {
    (n * (n + 1) / 2).pow(2)
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0006_one_to_ten() {
        assert_eq!(compute(10), 2640);
    }

    #[test]
    fn p0006_one_to_one_hundred() {
        assert_eq!(compute(100), 25164150);
    }
}
