// Project Euler: Problem 75

euler::run_solver!(75);

fn solve() -> String {
    compute(1_500_000).to_string()
}

fn compute(perim: usize) -> i64 {
    use euler::math;

    let limit = math::isqrt(perim as i64 / 2) as usize;
    let mut counter = vec![0; perim + 1];

    for m in 2..=limit {
        for n in ((1 + m % 2)..=m).step_by(2) {
            if math::gcd(m as i64, n as i64) == 1 {
                let p = 2 * m * (m + n);
                if p > perim {
                    break;
                }
                for i in (p..=perim).step_by(p) {
                    counter[i] += 1;
                }
            }
        }
    }
    counter.into_iter().filter(|&x| x == 1).count() as i64
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0075_48() {
        assert_eq!(compute(48), 6);
    }
    #[test]
    fn p0075_1500000() {
        assert_eq!(compute(1_500_000), 161667);
    }
}
