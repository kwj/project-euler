// Project Euler: Problem 80

euler::run_solver!(80);

fn solve() -> String {
    compute(100, 100).to_string()
}

fn compute(limit: i64, n_digits: u32) -> i64 {
    use euler::math;
    use num_bigint::BigUint;

    let e = (n_digits - 1) * 2;
    (1..=limit)
        .filter(|&x| math::isqrt(x).pow(2) != x)
        .map(|x| BigUint::from(10_u32).pow(e) * x as u32)
        .map(|x| {
            x.sqrt()
                .to_radix_le(10)
                .into_iter()
                .map(|y| y as i64)
                .sum::<i64>()
        })
        .sum::<i64>()
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
