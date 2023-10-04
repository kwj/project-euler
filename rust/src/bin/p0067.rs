// Project Euler: Problem 67

use std::fs::File;
use std::io::{BufReader, Lines};

euler::run_solver!(67);

fn solve() -> String {
    compute("./assets/p067_triangle.txt").to_string()
}

fn compute(fname: &str) -> i64 {
    let data_it = match euler::read_lines(fname) {
        Ok(it) => it,
        Err(error) => panic!("Problem reading the file {}: {:?}", fname, error),
    };
    let triangle = match parse_data(data_it) {
        Ok(hands) => hands,
        Err(error) => panic!("Problem parsing the file {}: {:?}", fname, error),
    };

    let mut prev = triangle[0].clone();
    for lst in &triangle[1..] {
        let selected = select_leaf(&prev);
        prev = lst
            .iter()
            .zip(selected.into_iter())
            .map(|(x, y)| x + y)
            .collect();
    }
    prev[0]
}

fn parse_data(it: Lines<BufReader<File>>) -> Result<Vec<Vec<i64>>, std::io::Error> {
    let mut ret: Vec<Vec<i64>> = Vec::new();

    for line_result in it {
        let line: Vec<i64> = line_result?
            .split_ascii_whitespace()
            .map(|s| s.parse::<i64>().unwrap())
            .collect();
        ret.insert(0, line);
    }
    Ok(ret)
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
        assert_eq!(compute("./assets/p067_triangle.txt"), 7273);
    }
}
