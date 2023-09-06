// Project Euler: Problem 19

euler::run_solver!(19);

fn solve() -> String {
    compute().to_string()
}

fn compute() -> i64 {
    let common_year: Vec<i64> = vec![31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
    let leap_year: Vec<i64> = vec![31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
    let mut days = repeat_vec(&concat_vec(&repeat_vec(&common_year, 3), &leap_year), 25);
    days.remove(days.len() - 1);

    let mut cumsum_days: Vec<i64> = vec![1 + 365];
    for d in days {
        cumsum_days.push(cumsum_days.last().unwrap() + d);
    }

    cumsum_days.into_iter().filter(|&x| x % 7 == 0).count() as i64
}

fn repeat_vec(v: &[i64], count: i64) -> Vec<i64> {
    let mut result: Vec<i64> = Vec::new();
    for _ in 0..count {
        result.extend(v);
    }
    result
}

fn concat_vec(v1: &[i64], v2: &[i64]) -> Vec<i64> {
    [v1, v2].concat()
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0019() {
        assert_eq!(compute(), 171);
    }
}
