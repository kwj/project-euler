// Project Euler: Problem 14

euler::run_solver!(14);

fn solve() -> String {
    compute(1_000_000).to_string()
}

fn compute(thr: i64) -> i64 {
    ((thr / 2)..thr)
        .max_by_key(|&n| get_collatz_length(n))
        .unwrap()
}

fn get_collatz_length(mut n: i64) -> i64 {
    let mut cnt: i64 = 1;
    while n > 1 {
        cnt += 1;
        if n & 1 == 0 {
            n /= 2;
        } else {
            n = 3 * n + 1;
        }
    }
    cnt
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0014_under_1000000() {
        assert_eq!(compute(1_000_000), 837799);
    }
}
