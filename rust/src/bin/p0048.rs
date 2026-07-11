// Project Euler: Problem 48

euler::run_solver!(48);

fn solve() -> String {
    compute(1000)
}

fn compute(upper: u64) -> String {
    use euler::math;

    debug_assert!(upper > 0);

    let m = 10_u64.pow(10);
    let ans = (1..=upper)
        .filter(|x| *x % 10 != 0)
        .map(|x| math::powmod(x, x, m))
        .sum::<u64>();

    format!("{:010}", ans % m)
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0047_10() {
        assert_eq!(compute(10), "0405071317");
    }

    #[test]
    fn p0047_1000() {
        assert_eq!(compute(1000), "9110846700");
    }
}
