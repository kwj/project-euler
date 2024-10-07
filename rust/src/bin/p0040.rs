// Project Euler: Problem 40

euler::run_solver!(40);

fn solve() -> String {
    compute().to_string()
}

fn compute() -> u32 {
    [1, 10, 100, 1_000, 10_000, 100_000, 1_000_000]
        .into_iter()
        .map(d)
        .product()
}

fn d(mut pos: u32) -> u32 {
    let mut n_digits = 1;

    while pos > n_digits * 9 * (10_u32.pow(n_digits - 1)) {
        pos -= n_digits * 9 * (10_u32.pow(n_digits - 1));
        n_digits += 1;
    }
    let q = (pos - 1) / n_digits;
    let r = (pos - 1) % n_digits;
    let num = 10_u32.pow(n_digits - 1) + q;

    (num / (10_u32.pow(n_digits - r - 1))) % 10
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0040() {
        assert_eq!(compute(), 210);
    }
}
