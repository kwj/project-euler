// Project Euler: Problem 76

euler::run_solver!(76);

fn solve() -> String {
    compute((1..100).collect(), 100).to_string()
}

fn compute(coins: Vec<usize>, target: usize) -> usize {
    let mut tbl = vec![0_usize; target + 1];
    tbl[0] = 1;

    for c in coins {
        for i in c..=target {
            tbl[i] += tbl[i - c];
        }
    }
    tbl[target]
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0076_5() {
        assert_eq!(compute((1..5).collect(), 5), 6);
    }
    #[test]
    fn p0076_100() {
        assert_eq!(compute((1..100).collect(), 100), 190569291);
    }
}
