// Project Euler: Problem 19

euler::run_solver!(19);

fn solve() -> String {
    compute().to_string()
}

fn compute() -> i64 {
    let common_year: Vec<i64> = vec![31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
    let leap_year: Vec<i64> = vec![31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];

    // number of days in each month (from 'Jan 1901' to 'Dec 2000')
    let mut days = repeat_vec(&concat_vec(&repeat_vec(&common_year, 3), &leap_year), 25);
    // insert the number of days in the year 1900 at the beginning
    days.insert(0, 365);
    // remove 'Dec 2000'
    days.pop();

    // the `initial_state` parameter of Iterator::scan() is the day of the week on January 1, 1900
    // (0: Sunday, 1: Monday, ..., 6: Saturday)
    days.iter()
        .scan(1, |state, &x| {
            *state = (*state + x) % 7;
            Some(*state)
        })
        .filter(|&x| x == 0)
        .count() as i64
}

fn repeat_vec(v: &[i64], count: usize) -> Vec<i64> {
    v.iter().copied().cycle().take(v.len() * count).collect()
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
