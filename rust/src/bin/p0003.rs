// Project Euler: Problem 3

euler::run_solver!(3);

fn solve() -> String {
    compute(600851475143).to_string()
}

fn compute(n: i64) -> i64 {
    use euler::math;

    math::factorize(n).last().unwrap().0
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0003_13195() {
        assert_eq!(compute(13195), 29);
    }

    #[test]
    fn p0003_600851475143() {
        assert_eq!(compute(600851475143), 6857);
    }
}
