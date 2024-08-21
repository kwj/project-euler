// Project Euler: Problem 99

euler::run_solver!(99);

static FILE_DATA: &str = include_str!("../../assets/0099_base_exp.txt");

fn solve() -> String {
    compute(FILE_DATA).to_string()
}

fn compute(data: &str) -> usize {
    let pairs = parse_data(data);
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

fn parse_data(data: &str) -> Vec<(f64, f64)> {
    let mut ret: Vec<(f64, f64)> = Vec::new();

    for line in data.lines() {
        let pair: Vec<_> = line.split(',').map(|s| s.parse::<f64>().unwrap()).collect();
        ret.push((pair[0], pair[1]));
    }
    ret
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0099() {
        assert_eq!(compute(super::FILE_DATA), 709);
    }
}
