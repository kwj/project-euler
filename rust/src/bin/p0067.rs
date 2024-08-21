// Project Euler: Problem 67

euler::run_solver!(67);

static FILE_DATA: &str = include_str!("../../assets/0067_triangle.txt");

fn solve() -> String {
    compute(FILE_DATA).to_string()
}

fn compute(data: &str) -> i64 {
    let triangle = parse_data(data);
    let mut prev = triangle[0].clone();

    for lst in &triangle[1..] {
        let selected = select_leaf(&prev);
        prev = lst
            .iter()
            .zip(selected.iter())
            .map(|(&x, &y)| x + y)
            .collect();
    }
    prev[0]
}

fn parse_data(data: &str) -> Vec<Vec<i64>> {
    let mut ret: Vec<Vec<i64>> = Vec::new();

    for line in data.lines() {
        let line: Vec<i64> = line
            .split_ascii_whitespace()
            .map(|s| s.parse::<i64>().unwrap())
            .collect();
        ret.insert(0, line);
    }
    ret
}

fn select_leaf(lst: &[i64]) -> Vec<i64> {
    use std::cmp;

    let mut result: Vec<i64> = Vec::new();
    let mut prev = lst[0];

    for i in lst.iter() {
        result.push(cmp::max(prev, *i));
        prev = *i;
    }
    result.remove(0);
    result
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0067() {
        assert_eq!(compute(super::FILE_DATA), 7273);
    }
}
