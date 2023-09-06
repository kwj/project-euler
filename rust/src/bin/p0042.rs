// Project Euler: Problem 42

euler::run_solver!(42);

fn solve() -> String {
    compute("./assets/p042_words.txt").to_string()
}

fn compute(fname: &str) -> i64 {
    use euler::math;

    let data = match euler::read_line(fname) {
        Err(error) => panic!("Problem reading the file {}: {:?}", fname, error),
        Ok(s) => s,
    };
    let names = parse_data(&data);

    names
        .into_iter()
        .map(|s| worth(&s) as i64)
        .filter(|&x| math::is_triangular(x))
        .count() as i64
}

fn parse_data(s: &str) -> Vec<String> {
    s.chars()
        .filter(|&c| c != '"')
        .collect::<String>()
        .split(',')
        .map(|s| s.to_string())
        .collect()
}

fn worth(s: &str) -> usize {
    s.chars().map(|c| c as usize - 'A' as usize + 1).sum()
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0042() {
        assert_eq!(compute("./assets/p042_words.txt"), 162);
    }
}
