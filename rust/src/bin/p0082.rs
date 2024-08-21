// Project Euler: Problem 81

euler::run_solver!(81);

static FILE_DATA: &str = include_str!("../../assets/0082_matrix.txt");

fn solve() -> String {
    compute(FILE_DATA).to_string()
}

fn compute(data: &str) -> i64 {
    use std::cmp;

    let matrix = parse_data(data);
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

fn parse_data(data: &str) -> Vec<Vec<i64>> {
    let mut ret: Vec<Vec<i64>> = Vec::new();

    for line in data.lines() {
        ret.push(line.split(',').map(|s| s.parse::<i64>().unwrap()).collect());
    }

    // transpose
    let n_row = ret[0].len();
    let mut its: Vec<_> = ret.into_iter().map(|row| row.into_iter()).collect();
    (0..n_row)
        .map(|_| its.iter_mut().map(|it| it.next().unwrap()).collect())
        .collect()
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0082() {
        assert_eq!(compute(super::FILE_DATA), 260324);
    }
}
