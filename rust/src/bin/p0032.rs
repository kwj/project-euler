// Project Euler: Problem 32

euler::run_solver!(32);

fn solve() -> String {
    compute().to_string()
}

fn compute() -> u64 {
    use euler::math;
    use itertools::Itertools;

    let mut nums: Vec<_> = make_cands()
        .into_iter()
        .filter(|(n, _)| math::is_pandigital_nz(*n))
        .map(|(_, prod)| prod)
        .collect();

    nums.sort_unstable();
    nums.iter().dedup().sum()
}

fn make_cands() -> Vec<(u64, u64)> {
    let mut lst: Vec<(u64, u64)> = Vec::new();

    for m1 in 1_000_u64..10_000 {
        for m2 in 2_u64..10 {
            if m1 * m2 < 10_000 {
                lst.push((m1 * 10_u64.pow(5) + m2 * 10_u64.pow(4) + m1 * m2, m1 * m2));
            }
        }
    }
    for m1 in 100_u64..1_000 {
        for m2 in 10_u64..100 {
            if m1 * m2 < 10_000 {
                lst.push((m1 * 10_u64.pow(6) + m2 * 10_u64.pow(4) + m1 * m2, m1 * m2));
            }
        }
    }

    lst
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0032() {
        assert_eq!(compute(), 45228);
    }
}
