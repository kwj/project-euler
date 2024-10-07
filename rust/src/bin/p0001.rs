// Project Euler: Problem 1

euler::run_solver!(1);

fn solve() -> String {
    compute(1000).to_string()
}

fn compute(limit: i64) -> i64 {
    debug_assert!(limit > 0);

    let f = |x: i64| -> i64 {
        // tmp: number of multiples of `x` less than `limit`
        let tmp = (limit - 1) / x;
        (1 + tmp) * tmp / 2 * x
    };

    f(3) + f(5) - f(15)
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0001_sum_less_than_10() {
        assert_eq!(compute(10), 23);
    }

    #[test]
    fn p0001_sum_less_than_1000() {
        assert_eq!(compute(1000), 233168);
    }
}
