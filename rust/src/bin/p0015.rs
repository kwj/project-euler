// Project Euler: Problem 15

euler::run_solver!(15);

fn solve() -> String {
    compute(20, 20).to_string()
}

fn compute(x: i64, y: i64) -> i64 {
    use euler::math;

    math::binomial(x + y, y)
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0015_grid_2_2() {
        assert_eq!(compute(2, 2), 6);
    }

    #[test]
    fn p0015_grid_20_20() {
        assert_eq!(compute(20, 20), 137846528820);
    }
}
