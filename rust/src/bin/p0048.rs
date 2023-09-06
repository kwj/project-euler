// Project Euler: Problem 48

euler::run_solver!(48);

fn solve() -> String {
    compute(1000)
}

fn compute(upper: i64) -> String {
    use euler::math;

    let ans = (1..=upper)
        .filter(|&x| x % 10 != 0)
        .map(|x| math::powmod(x, x, 10_i64.pow(10)))
        .sum::<i64>();

    format!("{:010}", ans % 10_i64.pow(10))
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
