// Project Euler: Problem 5

euler::run_solver!(5);

fn solve() -> String {
    compute(20).to_string()
}

fn compute(upper: i64) -> i64 {
    use euler::math;

    let mut answer = 1;
    for n in 2..=upper {
        answer = math::lcm(answer, n);
    }
    answer
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0005_upper_10() {
        assert_eq!(compute(10), 2520);
    }

    #[test]
    fn p0005_upper_20() {
        assert_eq!(compute(20), 232792560);
    }
}
