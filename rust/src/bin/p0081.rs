// Project Euler: Problem 81

euler::run_solver!(81);

static FILE_DATA: &str = include_str!("../../assets/0081_matrix.txt");

fn solve() -> String {
    compute(FILE_DATA).to_string()
}

fn compute(data: &str) -> i64 {
    use std::cmp;

    let mut matrix = parse_data(data);
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

fn parse_data(data: &str) -> Vec<Vec<i64>> {
    let mut ret: Vec<Vec<i64>> = Vec::new();

    for line in data.lines() {
        ret.push(line.split(',').map(|s| s.parse::<i64>().unwrap()).collect());
    }
    ret
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0081() {
        assert_eq!(compute(super::FILE_DATA), 427337);
    }
}
