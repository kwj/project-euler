// Project Euler: Problem 98

use std::cmp;
use std::collections::HashMap;

euler::run_solver!(98);

static FILE_DATA: &str = include_str!("../../assets/0098_words.txt");

fn solve() -> String {
    compute(FILE_DATA).to_string()
}

fn compute(data: &str) -> u64 {
    let mut word_tbl: HashMap<String, Vec<String>> = HashMap::new();
    let mut sq_tbl: HashMap<usize, Vec<String>> = HashMap::new();
    let mut ans: u64 = 0;

    for w in parse_data(data) {
        let mut tmp: Vec<_> = w.chars().collect();
        tmp.sort_unstable();
        let key: String = tmp.into_iter().collect();
        word_tbl.entry(key).or_default().push(w);
    }

    for anagram_words in word_tbl.values().filter(|words| words.len() > 1) {
        ans = cmp::max(ans, get_max_anagram(anagram_words, &mut sq_tbl));
    }

    ans
}

fn parse_data(s: &str) -> Vec<String> {
    if let Some(body) = s.get(1..(s.len() - 1)) {
        body.split(r#"",""#).map(|s| s.to_string()).collect()
    } else {
        unreachable!()
    }
}

fn get_max_anagram(words: &[String], tbl: &mut HashMap<usize, Vec<String>>) -> u64 {
    fn get_trans_tbl(w: &str, sq_str: &str) -> HashMap<char, char> {
        sq_str
            .chars()
            .zip(w.chars())
            .collect::<HashMap<char, char>>()
            .into_iter()
            .map(|(k, v)| (v, k))
            .collect()
    }

    fn aux(w1: &str, w2: &str, squares: &[String]) -> u64 {
        fn trans(tbl: &HashMap<char, char>, s: &str) -> String {
            s.chars()
                .map(|ch| {
                    if tbl.contains_key(&ch) {
                        *tbl.get(&ch).unwrap()
                    } else {
                        ch
                    }
                })
                .collect()
        }

        let mut ret: u64 = 0;
        for sq in squares {
            let trans_tbl = get_trans_tbl(w1, sq);
            if trans(&trans_tbl, w1) != *sq {
                continue;
            }

            let tmp = trans(&trans_tbl, w2);
            if !tmp.starts_with('0') && squares.contains(&tmp) {
                ret = cmp::max(ret, cmp::max(sq.parse().unwrap(), tmp.parse().unwrap()));
            }
        }
        ret
    }

    let mut ret: u64 = 0;
    let squares = get_squares(tbl, words[0].len());

    for (idx, w1) in words.iter().enumerate() {
        for w2 in &words[(idx + 1)..] {
            ret = cmp::max(ret, aux(w1, w2, &squares));
        }
    }
    ret
}

fn get_squares(tbl: &mut HashMap<usize, Vec<String>>, n_digits: usize) -> Vec<String> {
    if let Some(lst) = tbl.get(&n_digits) {
        lst.clone()
    } else {
        let mut lst: Vec<String> = Vec::new();
        for i in (10_u64.pow(n_digits as u32 - 1) - 1).isqrt() + 1
            ..=(10_u64.pow(n_digits as u32) - 1).isqrt()
        {
            lst.push(format!("{}", i * i));
        }
        tbl.insert(n_digits, lst.clone());
        lst
    }
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0098() {
        assert_eq!(compute(super::FILE_DATA), 18769);
    }
}
