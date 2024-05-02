// Project Euler: Problem 81

use std::fs::File;
use std::io::{BufReader, Lines};

euler::run_solver!(81);

fn solve() -> String {
    compute("./assets/p081_matrix.txt").to_string()
}

fn compute(fname: &str) -> i64 {
    use std::cmp;

    let data_it = match euler::read_lines(fname) {
        Ok(it) => it,
        Err(error) => panic!("Problem reading the file {}: {:?}", fname, error),
    };
    let mut matrix = match parse_data(data_it) {
        Ok(matrix) => matrix,
        Err(error) => panic!("Problem parsing the file {}: {:?}", fname, error),
    };

    let mut prev = matrix[0]
        .iter()
        .scan(0, |acc, x| {
            *acc += *x;
            Some(*acc)
        })
        .collect::<Vec<i64>>();
    prev.insert(0, i64::MAX);

    for work in matrix[1..].iter_mut() {
        (*work).insert(0, i64::MAX);
        for i in 1..((*work).len()) {
            work[i] += cmp::min(work[i - 1], prev[i]);
        }
        prev.clone_from(work);
    }
    *prev.last().unwrap()
}

fn parse_data(it: Lines<BufReader<File>>) -> Result<Vec<Vec<i64>>, std::io::Error> {
    let mut ret: Vec<Vec<i64>> = Vec::new();

    for line_result in it {
        let row = line_result?
            .split(',')
            .map(|s| s.parse::<i64>().unwrap())
            .collect();
        ret.push(row);
    }
    Ok(ret)
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0081() {
        assert_eq!(compute("./assets/p081_matrix.txt"), 427337);
    }
}
