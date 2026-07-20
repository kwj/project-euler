// Project Euler: Problem 67

euler::run_solver!(67);

static FILE_DATA: &str = include_str!("../../assets/0067_triangle.txt");

fn solve() -> String {
    compute(FILE_DATA).to_string()
}

fn compute(data: &str) -> u64 {
    parse_data(data)
        .into_iter()
        .rev()
        .reduce(|acc, e| {
            acc.windows(2)
                .map(|v| v[0].max(v[1]))
                .zip(e)
                .map(|(e1, e2)| e1 + e2)
                .collect()
        })
        .unwrap()[0]
}

fn parse_data(data: &str) -> Vec<Vec<u64>> {
    data.lines()
        .map(|s| {
            s.split_ascii_whitespace()
                .map(|elm| elm.parse::<u64>().unwrap())
                .collect()
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0067() {
        assert_eq!(compute(super::FILE_DATA), 7273);
    }
}
