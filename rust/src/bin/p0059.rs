// Project Euler: Problem 59

euler::run_solver!(59);

static FILE_DATA: &str = include_str!("../../assets/0059_cipher.txt");

fn solve() -> String {
    compute(FILE_DATA).to_string()
}

fn compute(data: &str) -> i64 {
    use itertools::Itertools;

    let cipher_text = parse_data(data);
    let mut ans = 0_i64;
    let mut max_score = 0_i64;

    // 'a' = 0x61, 'z' = 0x7A
    for key in (0..3).map(|_| 0x61_u8..=0x7A).multi_cartesian_product() {
        let decrypted_text: Vec<_> = cipher_text
            .iter()
            .enumerate()
            .map(|(idx, n)| n ^ key[idx % 3])
            .collect();
        if decrypted_text
            .iter()
            .map(|&x| char::from(x)) // without this code, it is about 10% slower on my machine
            .all(|c| c.is_ascii_graphic() || c.is_ascii_whitespace())
        {
            let score = decrypted_text.iter().map(|ch| calc_score(*ch)).sum();
            if score > max_score {
                max_score = score;
                ans = decrypted_text.into_iter().map(i64::from).sum();
            }
        }
    }

    ans
}

fn parse_data(s: &str) -> Vec<u8> {
    s.split(',').map(|x| x.parse::<u8>().unwrap()).collect()
}

/*
  valuation rules:
    0x20 (space): 4
    0x41 - 0x5A (uppercase letters, 'A' - 'Z'): 3
    0x61 - 0x7A (lowercase letters, 'a' - 'z'): 5
    0x21 - 0x7E (printable characters except alphabet letters): 1
    others: 0
*/
fn calc_score(ch: u8) -> i64 {
    if ch == 0x20 {
        return 4;
    }
    if (0x41..=0x5A).contains(&ch) {
        return 3;
    }
    if (0x61..=0x7A).contains(&ch) {
        return 5;
    }
    if (0x21..=0x7E).contains(&ch) {
        return 1;
    }
    0
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0059() {
        assert_eq!(compute(super::FILE_DATA), 129448);
    }
}
