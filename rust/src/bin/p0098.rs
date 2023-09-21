// Project Euler: Problem 98

use std::cmp;
use std::collections::HashMap;

euler::run_solver!(98);

fn solve() -> String {
    compute("./assets/p098_words.txt").to_string()
}

fn compute(fname: &str) -> i64 {
    let data = match euler::read_line(fname) {
        Err(error) => panic!("Problem reading the file {}: {:?}", fname, error),
        Ok(s) => s,
    };

    let mut word_tbl: HashMap<String, Vec<String>> = HashMap::new();
    for w in parse_data(&data) {
        let mut tmp: Vec<_> = w.chars().collect();
        tmp.sort();
        let key: String = tmp.into_iter().collect();
        word_tbl.entry(key).or_insert(Vec::new()).push(w);
    }

    let mut ans: i64 = 0;
    let mut sq_tbl: HashMap<usize, Vec<String>> = HashMap::new();
    for anagram_words in word_tbl.values().filter(|&words| words.len() > 1) {
        ans = cmp::max(ans, get_max_anagram(anagram_words, &mut sq_tbl));
    }
    ans
}

fn parse_data(s: &str) -> Vec<String> {
    s.chars()
        .filter(|&c| c != '"')
        .collect::<String>()
        .split(',')
        .map(|s| s.to_string())
        .collect()
}

fn get_max_anagram(words: &[String], tbl: &mut HashMap<usize, Vec<String>>) -> i64 {
    // It depends on the behavior of HashMap::from_iter() to overwrite the value
    // in case of duplicate keys. It probably this behavior is made the same as insert(),
    // but I could not confirm this in the documentation.
    fn get_trans_tbl(w: &str, sq_str: &str) -> HashMap<char, char> {
        HashMap::from_iter(
            (HashMap::from_iter(sq_str.chars().zip(w.chars())) as HashMap<char, char>)
                .into_iter()
                .map(|(k, v)| (v, k)),
        )
    }

    fn aux(w1: &str, w2: &str, squares: &[String]) -> i64 {
        fn trans(tbl: &HashMap<char, char>, s: &str) -> String {
            let ret = s
                .chars()
                .map(|ch| {
                    if tbl.contains_key(&ch) {
                        *tbl.get(&ch).unwrap()
                    } else {
                        ch
                    }
                })
                .collect::<String>();
            ret
        }

        let mut ret: i64 = 0;
        for sq in squares.iter() {
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

    let mut ret: i64 = 0;
    let squares = get_squares(tbl, words[0].len());

    for (idx, w1) in words.iter().enumerate() {
        for w2 in words[(idx + 1)..].iter() {
            ret = cmp::max(ret, aux(w1, w2, &squares));
        }
    }
    ret
}

fn get_squares(tbl: &mut HashMap<usize, Vec<String>>, n_digits: usize) -> Vec<String> {
    use euler::math;

    if let Some(lst) = tbl.get(&n_digits) {
        lst.clone()
    } else {
        let mut lst: Vec<String> = Vec::new();
        for i in (math::isqrt(10_i64.pow(n_digits as u32 - 1) - 1) + 1)
            ..=math::isqrt(10_i64.pow(n_digits as u32) - 1)
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
        assert_eq!(compute("./assets/p098_words.txt"), 18769);
    }
}
