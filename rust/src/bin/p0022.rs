// Project Euler: Problem 22

euler::run_solver!(22);

static FILE_DATA: &str = include_str!("../../assets/0022_names.txt");

fn solve() -> String {
    compute(FILE_DATA).to_string()
}

fn compute(data: &str) -> usize {
    let mut names = parse_data(data);

    names.sort();
    names
        .iter()
        .enumerate()
        .map(|(idx, word)| (idx + 1) * worth(word))
        .sum()
}

fn parse_data(s: &str) -> Vec<String> {
    use std::string::ToString;

    s.chars()
        .filter(|&c| c != '"')
        .collect::<String>()
        .split(',')
        .map(ToString::to_string)
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
