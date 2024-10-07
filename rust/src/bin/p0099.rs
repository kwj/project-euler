// Project Euler: Problem 99

euler::run_solver!(99);

static FILE_DATA: &str = include_str!("../../assets/0099_base_exp.txt");

fn solve() -> String {
    compute(FILE_DATA).to_string()
}

fn compute(data: &str) -> usize {
    let max_line = parse_data(data)
        .into_iter()
        .map(|(b, e)| e * b.ln())
        .enumerate()
        .max_by(|(_, x1), (_, x2)| x1.total_cmp(x2))
        .unwrap();

    max_line.0 + 1
}

fn parse_data(data: &str) -> Vec<(f64, f64)> {
    use itertools::Itertools;

    data.lines()
        .map(|line| {
            line.split(',')
                .map(|s| s.parse::<f64>().unwrap())
                .collect_tuple()
                .unwrap()
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0099() {
        assert_eq!(compute(super::FILE_DATA), 709);
    }
}
