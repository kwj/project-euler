// Project Euler: Problem 81

euler::run_solver!(81);

static FILE_DATA: &str = include_str!("../../assets/0081_matrix.txt");

fn solve() -> String {
    compute(FILE_DATA).to_string()
}

fn compute(data: &str) -> u64 {
    use std::cmp;

    let mut matrix = parse_data(data);
    let mut prev: Vec<_> = matrix[0]
        .iter()
        .scan(0, |acc, x| {
            *acc += *x;
            Some(*acc)
        })
        .collect();
    prev.insert(0, u64::MAX);

    for work in &mut matrix[1..] {
        (*work).insert(0, u64::MAX);
        for i in 1..((*work).len()) {
            work[i] += cmp::min(work[i - 1], prev[i]);
        }
        prev.clone_from(work);
    }

    *prev.last().unwrap()
}

fn parse_data(data: &str) -> Vec<Vec<u64>> {
    let mut ret: Vec<Vec<u64>> = Vec::new();

    for line in data.lines() {
        ret.push(line.split(',').map(|s| s.parse::<u64>().unwrap()).collect());
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
