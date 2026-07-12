// Project Euler: Problem 89

/*
  step 1:
    IIIIIIIII     IX
    XXXXXXXXX     XC
    CCCCCCCCC     CM

  step 2:
    VIIII         IX
    LXXXX         XC
    DCCCC         CM

  step 3:
    IIIII         V
    XXXXX         L
    CCCCC         D

  step 4:
    IIII          IV
    XXXX          XL
    CCCC          CD
*/

euler::run_solver!(89);

static FILE_DATA: &str = include_str!("../../assets/0089_roman.txt");

fn solve() -> String {
    compute(FILE_DATA).to_string()
}

fn compute(data: &str) -> usize {
    parse_data(data)
        .into_iter()
        .map(|s| s.len() - replace_numbers(&s).len())
        .sum()
}

fn replace_numbers(s: &str) -> String {
    let from_to = [
        ("IIIIIIIII", "##"),
        ("XXXXXXXXX", "##"),
        ("CCCCCCCCC", "##"),
        ("VIIII", "##"),
        ("LXXXX", "##"),
        ("DCCCC", "##"),
        ("IIIII", "#"),
        ("XXXXX", "#"),
        ("CCCCC", "#"),
        ("IIII", "##"),
        ("XXXX", "##"),
        ("CCCC", "##"),
    ];

    from_to
        .into_iter()
        .fold(s.to_string(), |acc, (from, to)| acc.replace(from, to))
}

fn parse_data(data: &str) -> Vec<String> {
    let mut ret: Vec<String> = Vec::new();

    for line in data.lines() {
        ret.push(line.to_string());
    }

    ret
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0089() {
        assert_eq!(compute(super::FILE_DATA), 743);
    }
}
