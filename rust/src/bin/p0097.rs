// Project Euler: Problem 97

euler::run_solver!(97);

fn solve() -> String {
    compute(10)
}

fn compute(n_digits: u32) -> String {
    use euler::math;

    let divisor = 10_i64.pow(n_digits);
    format!(
        "{:0width$}",
        (28433 * math::powmod(2, 7830457, divisor) + 1) % divisor,
        width = n_digits as usize
    )
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0097() {
        assert_eq!(compute(10), "8739992577");
    }
}
