// Project Euler: Problem 22

euler::run_solver!(22);

static FILE_DATA: &str = include_str!("../../assets/0022_names.txt");

fn solve() -> String {
    compute(FILE_DATA).to_string()
}

fn compute(data: &str) -> i64 {
    let mut names = parse_data(data);

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
        assert_eq!(compute(super::FILE_DATA), 871198282);
    }
}
