// Project Euler: Problem 40

euler::run_solver!(40);

fn solve() -> String {
    compute().to_string()
}

fn compute() -> i64 {
    d(1) * d(10) * d(100) * d(1_000) * d(10_000) * d(100_000) * d(1_000_000)
}

fn d(mut pos: i64) -> i64 {
    let mut n_digits = 1;
    while pos > n_digits * 9 * (10_i64.pow(n_digits as u32 - 1)) {
        pos -= n_digits * 9 * (10_i64.pow(n_digits as u32 - 1));
        n_digits += 1;
    }
    let q = (pos - 1) / n_digits;
    let r = (pos - 1) % n_digits;
    let num = 10_i64.pow(n_digits as u32 - 1) + q;

    (num / (10_i64.pow((n_digits - r - 1) as u32))) % 10
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0040() {
        assert_eq!(compute(), 210);
    }
}
