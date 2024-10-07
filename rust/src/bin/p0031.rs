// Project Euler: Problem 31

euler::run_solver!(31);

fn solve() -> String {
    compute(vec![1, 2, 5, 10, 20, 50, 100, 200], 200).to_string()
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
    fn p0031() {
        assert_eq!(compute(vec![1, 2, 5, 10, 20, 50, 100, 200], 200), 73682);
    }
}
