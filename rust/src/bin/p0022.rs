// Project Euler: Problem 22

euler::run_solver!(22);

fn solve() -> String {
    compute("./assets/p022_names.txt").to_string()
}

fn compute(fname: &str) -> i64 {
    let data = match euler::read_line(fname) {
        Err(error) => panic!("Problem reading the file {}: {:?}", fname, error),
        Ok(s) => s,
    };
    let mut names = parse_data(&data);

    names.sort();
    names
        .into_iter()
        .enumerate()
        .map(|(idx, word)| (idx + 1) * worth(&word))
        .sum::<usize>() as i64
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
    fn p0022() {
        assert_eq!(compute("./assets/p022_names.txt"), 871198282);
    }
}
