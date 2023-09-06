// Project Euler: Problem 81

use std::fs::File;
use std::io::{BufReader, Lines};

euler::run_solver!(81);

fn solve() -> String {
    compute("./assets/p082_matrix.txt").to_string()
}

fn compute(fname: &str) -> i64 {
    use std::cmp;

    let data_it = match euler::read_lines(fname) {
        Ok(it) => it,
        Err(error) => panic!("Problem reading the file {}: {:?}", fname, error),
    };
    let matrix = match parse_data(data_it) {
        Ok(matrix) => matrix,
        Err(error) => panic!("Problem parsing the file {}: {:?}", fname, error),
    };

    let mut work = matrix[0].clone();
    for crnt in matrix[1..].iter() {
        work[0] += crnt[0];
        for i in 1..(crnt.len()) {
            work[i] = crnt[i] + cmp::min(work[i], work[i - 1]);
        }
        for i in (0..(crnt.len() - 1)).rev() {
            work[i] = cmp::min(work[i], work[i + 1] + crnt[i]);
        }
    }

    work.sort();
    work[0]
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

    // transpose
    let n_row = ret[0].len();
    let mut its: Vec<_> = ret.into_iter().map(|row| row.into_iter()).collect();
    Ok((0..n_row)
        .map(|_| its.iter_mut().map(|it| it.next().unwrap()).collect())
        .collect())
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0082() {
        assert_eq!(compute("./assets/p082_matrix.txt"), 260324);
    }
}
