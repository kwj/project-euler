// Project Euler: Problem 1

euler::run_solver!(1);

fn solve() -> String {
    compute(1000).to_string()
}

fn compute(mut limit: i64) -> i64 {
    limit -= 1;
    sum_of_mults(3, limit) + sum_of_mults(5, limit) - sum_of_mults(15, limit)
}

fn sum_of_mults(n: i64, ulimit: i64) -> i64 {
    let tmp = ulimit / n;
    (1 + tmp) * tmp / 2 * n
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
