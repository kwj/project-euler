// Project Euler: Problem 42

euler::run_solver!(42);

static FILE_DATA: &str = include_str!("../../assets/0042_words.txt");

fn solve() -> String {
    compute(FILE_DATA).to_string()
}

fn compute(data: &str) -> usize {
    use euler::math;

    parse_data(data)
        .into_iter()
        .filter(|s| math::is_triangular(worth(s)))
        .count()
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

fn worth(s: &str) -> i64 {
    s.chars().map(|c| c as i64 - 'A' as i64 + 1).sum()
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0042() {
        assert_eq!(compute(super::FILE_DATA), 162);
    }
}
