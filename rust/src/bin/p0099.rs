// Project Euler: Problem 99

use std::fs::File;
use std::io::{BufReader, Lines};

euler::run_solver!(99);

fn solve() -> String {
    compute("./assets/p099_base_exp.txt").to_string()
}

fn compute(fname: &str) -> usize {
    let data_it = match euler::read_lines(fname) {
        Ok(it) => it,
        Err(error) => panic!("Problem reading the file {}: {:?}", fname, error),
    };
    let pairs = match parse_data(data_it) {
        Ok(pairs) => pairs,
        Err(error) => panic!("Problem parsing the file {}: {:?}", fname, error),
    };

    let mut ans: usize = 0;
    let mut max_value: f64 = 0.0;
    for (idx, (base, exp)) in pairs.into_iter().enumerate() {
        let tmp = exp * base.log10();
        if tmp > max_value {
            ans = idx;
            max_value = tmp;
        }
    }
    ans + 1
}

fn parse_data(it: Lines<BufReader<File>>) -> Result<Vec<(f64, f64)>, std::io::Error> {
    let mut ret: Vec<(f64, f64)> = Vec::new();

    for line_result in it {
        let pair: Vec<_> = line_result?
            .split(',')
            .map(|s| s.parse::<f64>().unwrap())
            .collect();
        ret.push((pair[0], pair[1]));
    }
    Ok(ret)
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0099() {
        assert_eq!(compute("./assets/p099_base_exp.txt"), 709);
    }
}
