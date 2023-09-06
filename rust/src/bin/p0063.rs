// Project Euler: Problem 63

euler::run_solver!(63);

fn solve() -> String {
    compute().to_string()
}

fn compute() -> i64 {
    (1..=9)
        .map(|m| ((1.0 / (1.0 - (m as f32).log10())).floor()) as i64)
        .sum()
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0063() {
        assert_eq!(compute(), 49);
    }
}
