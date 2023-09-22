// Project Euler: Problem 68

/*
  ring: Vec<i64>

          r1
            \
            r0  [r6 <- r0]
           /  \
         r4---r2-r3
         /
       r5

          r1
            \  [r10 <- r0]
            r0  r3
           /  \ /
         r8   r2
         /\   /
      r9 r6-r4-r5
            \
             r7
*/

euler::run_solver!(68);

fn solve() -> String {
    compute(5).to_string()
}

fn compute(n_gon: i64) -> i64 {
    solve_by_backtraking(n_gon)
}

fn solve_by_backtraking(n_gon: i64) -> i64 {
    let mut ring = vec![0_i64; (n_gon * 2 + 1) as usize];
    let mut result: Vec<String> = Vec::new();

    // The minimum total of the line on '10' exists is 1 + 2 + (n_gon * 2) = n_gon * 2 + 3.
    // The maximum total of the line on '1' exsits is 1 + (n_gon * 2 - 1) + (n_gon * 2) = n_gon * 4.
    for total in (n_gon * 2 + 3)..=(n_gon * 4) {
        for n in 1..=(n_gon * 2) {
            ring[0] = n;
            ring[n_gon as usize * 2] = n;
            dfs(n_gon, 0, 1 << n, &mut ring, total, &mut result);
        }
    }

    // Only 16-digit strings are covered in this problem (when n_gon == 5).
    if n_gon == 5 {
        result.retain(|s| s.len() == 16);
    }

    result
        .into_iter()
        .map(|s| s.parse().unwrap())
        .max()
        .unwrap()
}

// bit_mask: 1: used number, 0: unused number
//  0x11111111110
//    ^        ^
//    10  ...  1
fn dfs(
    n_gon: i64,
    mut idx: usize,
    bit_mask: usize,
    ring: &mut Vec<i64>,
    total: i64,
    result: &mut Vec<String>,
) {
    let start_pos = 1;
    if idx == n_gon as usize * 2 - 2 {
        let tmp = total - ring[0] - ring[idx];
        if (0 < tmp && tmp <= n_gon * 2) && tmp > ring[start_pos] && ((1 << tmp) & bit_mask == 0) {
            ring[idx + 1] = tmp;
            let mut s = String::from("");
            loop {
                s = format!("{}{}{}{}", ring[idx + 1], ring[idx], ring[idx + 2], s);
                if idx == 0 {
                    break;
                }
                idx -= 2;
            }
            result.push(s);
        }
    }

    for external_node in 1..=(n_gon * 2) {
        let internal_node = total - ring[idx] - external_node;
        if !is_valid(external_node, internal_node, bit_mask, n_gon) {
            continue;
        }
        ring[idx + 1] = external_node;
        ring[idx + 2] = internal_node;

        // pruning: if starting node is the smallest external node, continue to search
        if ring[start_pos] <= external_node {
            dfs(
                n_gon,
                idx + 2,
                (1 << external_node) | (1 << internal_node) | bit_mask,
                ring,
                total,
                result,
            );
        }
    }
}

fn is_valid(x: i64, y: i64, bit_mask: usize, n_gon: i64) -> bool {
    0 < x
        && x <= (n_gon * 2)
        && 0 < y
        && y <= (n_gon * 2)
        && x != y
        && ((1 << x) | (1 << y)) & bit_mask == 0
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0068_3() {
        assert_eq!(compute(3), 432621513);
    }

    #[test]
    fn p0068_5() {
        assert_eq!(compute(5), 6531031914842725);
    }
}
