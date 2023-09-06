// Project Euler: Problem 39

euler::run_solver!(39);

fn solve() -> String {
    compute(1_000).to_string()
}

fn compute(limit: i64) -> i64 {
    let mut result: Vec<(usize, i64)> = Vec::new();

    for p in (2..=limit).step_by(2) {
        let mut lst: Vec<(i64, i64, i64)> = Vec::new();
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

    result.sort_by(|&a, &b| (b.0).cmp(&(a.0)));
    result[0].1
}

fn check_pair(p: i64, a: i64) -> bool {
    (p * p - 2 * a * p) % (2 * (p - a)) == 0
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0038_limit_1000() {
        assert_eq!(compute(1_000), 840);
    }
}
