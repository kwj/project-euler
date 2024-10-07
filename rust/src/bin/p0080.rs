// Project Euler: Problem 80

euler::run_solver!(80);

fn solve() -> String {
    compute(100, 100).to_string()
}

fn compute(limit: i64, n_digits: u32) -> i64 {
    use euler::math;
    use num_bigint::BigUint;

    debug_assert!(limit > 1);

    let power_10 = BigUint::from(10_u32).pow((n_digits - 1) * 2);
    (2..=limit)
        .filter(|&x| !math::is_square(x))
        .map(|x| (power_10.clone() * x as u32).sqrt())
        .map(|x| {
            x.to_radix_le(10)
                .into_iter()
                .rev()
                .take(n_digits as usize)
                .map(i64::from)
                .sum::<i64>()
        })
        .sum()
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0080_2() {
        assert_eq!(compute(2, 100), 475);
    }

    #[test]
    fn p0080_100() {
        assert_eq!(compute(100, 100), 40886);
    }
}
