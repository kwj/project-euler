// Project Euler: Problem 6

euler::run_solver!(6);

fn solve() -> String {
    compute(1, 100).to_string()
}

fn compute(start: i64, stop: i64) -> i64 {
    let sum_of_squares = (start..=stop).map(|x| x.pow(2)).sum::<i64>();
    let square_of_sum = (start..=stop).sum::<i64>().pow(2);

    (sum_of_squares - square_of_sum).abs()
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0006_one_to_ten() {
        assert_eq!(compute(1, 10), 2640);
    }

    #[test]
    fn p0006_one_to_one_hundred() {
        assert_eq!(compute(1, 100), 25164150);
    }
}
