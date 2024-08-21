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

use regex::Regex;

euler::run_solver!(89);

static FILE_DATA: &str = include_str!("../../assets/0089_roman.txt");

fn solve() -> String {
    compute(FILE_DATA).to_string()
}

fn compute(data: &str) -> usize {
    let lines = parse_data(data);
    let re_step1 = Regex::new(r"IIIIIIIII|XXXXXXXXX|CCCCCCCCC").unwrap();
    let re_step2 = Regex::new(r"VIIII|LXXXX|DCCCC").unwrap();
    let re_step3 = Regex::new(r"IIIII|XXXXX|CCCCC").unwrap();
    let re_step4 = Regex::new(r"IIII|XXXX|CCCC").unwrap();

    lines
        .iter()
        .map(|s| s.len() - replace_numbers(s, &re_step1, &re_step2, &re_step3, &re_step4).len())
        .sum()
}

fn replace_numbers(line: &str, re1: &Regex, re2: &Regex, re3: &Regex, re4: &Regex) -> String {
    let s1 = re1.replace_all(line, "##");
    let s2 = re2.replace_all(&s1, "##");
    let s3 = re3.replace_all(&s2, "#");
    re4.replace_all(&s3, "##").to_string()
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
