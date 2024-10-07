// Project Euler: Problem 16

euler::run_solver!(16);

fn solve() -> String {
    compute(1_000).to_string()
}

fn compute(exp: u32) -> i64 {
    use num_bigint::BigUint;

    BigUint::from(2_u32)
        .pow(exp)
        .to_radix_le(10)
        .into_iter()
        .map(i64::from)
        .sum()
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0016_exp_15() {
        assert_eq!(compute(15), 26);
    }

    #[test]
    fn ep0016_exp_1000() {
        assert_eq!(compute(1_000), 1366);
    }
}
