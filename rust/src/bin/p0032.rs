// Project Euler: Problem 32

euler::run_solver!(32);

fn solve() -> String {
    compute().to_string()
}

fn compute() -> u64 {
    use euler::math;
    use itertools::Itertools;

    make_cands()
        .into_iter()
        .filter_map(|(n, prod)| {
            if math::is_pandigital_nz(n) {
                Some(prod)
            } else {
                None
            }
        })
        .sorted()
        .dedup()
        .sum()
}

fn make_cands() -> Vec<(u64, u64)> {
    let it1 = (1_000_u64..10_000).flat_map(|m1| {
        (2_u64..10)
            .map(move |m2| (m1 * 10_u64.pow(5) + m2 * 10_u64.pow(4) + m1 * m2, m1 * m2))
            .take_while(|(_, x)| *x < 10_000)
    });
    let it2 = (100_u64..1_000).flat_map(|m1| {
        (10_u64..100)
            .map(move |m2| (m1 * 10_u64.pow(6) + m2 * 10_u64.pow(4) + m1 * m2, m1 * m2))
            .take_while(|(_, x)| *x < 10_000)
    });

    it1.chain(it2).collect()
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0032() {
        assert_eq!(compute(), 45228);
    }
}
