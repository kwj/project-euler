// Project Euler: Problem 43

euler::run_solver!(43);

fn solve() -> String {
    compute().to_string()
}

fn compute() -> i64 {
    let mut lst: Vec<String> = vec!["".to_string()];

    for d in [1, 1, 17, 13, 11, 7, 5, 3, 2, 1] {
        let mut tmp_lst: Vec<String> = Vec::new();
        for x in "0123456789".chars() {
            for s in &lst {
                if !s.contains(x) {
                    let tmp_s = format!("{x}{s}");
                    if tmp_s.len() < 3 || tmp_s[0..3].parse::<i32>().unwrap() % d == 0 {
                        tmp_lst.push(tmp_s);
                    }
                }
            }
        }
        lst = tmp_lst;
    }

    lst.into_iter()
        .filter(|s| s.as_bytes()[0] != 0x30) // Check if the leftmost digit isn't zero.
        .filter_map(|s| s.parse::<i64>().ok())
        .sum()
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0043() {
        assert_eq!(compute(), 16695334890);
    }
}
