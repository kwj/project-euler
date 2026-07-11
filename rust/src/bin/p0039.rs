// Project Euler: Problem 39

euler::run_solver!(39);

fn solve() -> String {
    compute(1_000).to_string()
}

fn compute(limit: u64) -> u64 {
    // the smallest right-angle triangle with integral length sides is {3, 4, 5}
    debug_assert!(limit >= 12);

    let mut result: Vec<(usize, u64)> = Vec::new();

    for p in (2..=limit).step_by(2) {
        let mut lst: Vec<(u64, u64, u64)> = Vec::new();
        for a in 1..=((p - 1) / 3) {
            if check_pair(p, a) {
                let b = (p * p - 2 * a * p) / (2 * (p - a));
                lst.push((a, b, p - a - b));
            }
        }
        if !lst.is_empty() {
            result.push((lst.len(), p));
        }
    }

    result.into_iter().max_by_key(|x| x.0).unwrap().1
}

fn check_pair(p: u64, a: u64) -> bool {
    (p * p - 2 * a * p).is_multiple_of(2 * (p - a))
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0038_limit_1000() {
        assert_eq!(compute(1_000), 840);
    }
}
