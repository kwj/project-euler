// Project Euler: Problem 24

euler::run_solver!(24);

fn solve() -> String {
    compute(1_000_000)
}

fn compute(mut idx: usize) -> String {
    let mut lst: Vec<i64> = vec![0, 1, 2, 3, 4, 5, 6, 7, 8, 9];
    let mut acc = 0_i64;
    idx -= 1;

    for i in (0..lst.len()).rev() {
        let fact = factorial(i);
        let blk = idx / fact;
        idx %= fact;
        acc = acc * 10 + lst[blk];
        lst.remove(blk);
    }
    format!("{:010}", acc)
}

fn factorial(n: usize) -> usize {
    if n > 1 {
        n * factorial(n - 1)
    } else {
        1
    }
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0024_1_000_000() {
        assert_eq!(compute(1_000_000), "2783915460");
    }
}
