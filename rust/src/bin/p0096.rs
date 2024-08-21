// Project Euler: Problem 96

/*
  This solution uses a simple backtracking algorithm.

      C0 C1 C2 C3 C4 C5 C6 C7 C8
     +--------+--------+--------+    R: Row
  R0 | 0  1  2| 3  4  5| 6  7  8|    C: Column
  R1 | 9 10 11|12      |15      |
  R2 |18      |21      |24      |    Cell: 0..=80
     +--------+--------+--------+
  R3 |27      |30      |33      |
  R4 |36      |39      |42      |
  R5 |45      |48      |51      |
     +--------+--------+--------+
  R6 |54      |57      |60      |
  R7 |63      |66      |69      |
  R8 |72      |75 76 77|78 79 80|
     +--------+--------+--------+

  grid: [u128; 10]
    grid[n] is the information of number 'n'

    127  81 80                   0
    |......|.....................|
     unused       bit table

  Example:
   * Grid 01 (from p096_sudoku.txt)
      003|020|600
      900|305|001
      001|806|400
      ---+---+---
      008|102|900
      700|000|008
      006|708|200
      ---+---+---
      002|609|500
      800|203|009
      005|010|300

    Initial Data: grid[]
      [0x1ab6b64f26fec9e4dadab,
       0x010000000000040120000, 0x000040108000100000010, 0x040100000000000001004,
       0x000000000000001000000, 0x004001000000000004000, 0x000000200800000800040,
       0x000000001001000000000, 0x000008004100020200000, 0x000800800000200000200]

    1) number '0'
        python> bin(grid[0])[::-1][0:10]    # print only 'R0'
        '110101011'

        R0: 003|020|600
           '110 101 011'

    2) number '3'
        python> bin(grid[3])[::-1][0:10]    # print only 'R0'
        '001000000'

        R0: 003|020|600
           '001 000 000'

   * Grid 01 (Solution)
      483|921|657
      967|345|821
      251|876|493
      ---+---+---
      548|132|976
      729|564|138
      136|798|245
      ---+---+---
      372|689|514
      814|253|769
      695|417|382

    Solution: sol[]
      [0x1ab6b64f26fec9e4dadab,
       0x010012000240040120020, 0x100040108002100050010, 0x040100040480084001004,
       0x008024010020011002001, 0x004081020008008084080, 0x001400200810800800440,
       0x020200081001400400900, 0x080008404100020208002, 0x002800802004202000208]

    1) number '4'
      python> "{:081b}".format(sol[4])[::-1]
     '100000000000010000000000100010000000000001000000000010000000001001000000000100000'
      python> s = "{:081b}".format(sol[4])[::-1]
      python> for i in range(0, len(s), 9):
                  print(s[i:i+9])

      100000000
      000010000
      000000100
      010000000
      000001000
      000000010
      000000001
      001000000
      000100000

        The set bits indicate that there is a number at position. In this case, the number is 4.
        Please compare it with the solution of Grid 01.
*/

use regex::Regex;

euler::run_solver!(96);

static FILE_DATA: &str = include_str!("../../assets/0096_sudoku.txt");

const NUM_OF_CELLS: usize = 81;

// 0b: 111111111
const ROW_MASK: u128 = 0x1ff;
// 0b: 000000001 000000001 ... 000000001
const COL_MASK: u128 = 0x1008040201008040201;
// 0b: 000000111 000000111 000000111
const BOX_MASK: u128 = 0x1c0e07;

const ADJACENT_CELLS: [u128; NUM_OF_CELLS] = {
    let mut tbl = [0_u128; NUM_OF_CELLS];

    let mut pos: usize = 0;
    while pos < NUM_OF_CELLS {
        let row_shift = (pos / 9) * 9;
        let col_shift = pos % 9;
        let box_shift = (pos / 27) * 27 + (col_shift / 3) * 3;
        tbl[pos] |= ROW_MASK << row_shift;
        tbl[pos] |= COL_MASK << col_shift;
        tbl[pos] |= BOX_MASK << box_shift;
        pos += 1;
    }
    tbl
};

fn solve() -> String {
    compute(FILE_DATA).to_string()
}

fn compute(data: &str) -> usize {
    let puzzles = parse_data(data);
    let grids = check_data(&puzzles);

    let mut ans: usize = 0;
    for grid in grids {
        if let Some(sol) = sudoku_solover(&grid) {
            let mut acc: usize = 0;
            for i in 0..3 {
                let bit: u128 = 1 << i;
                for (pos, elm) in sol.iter().enumerate().skip(1) {
                    if *elm & bit != 0 {
                        acc = acc * 10 + pos;
                        break;
                    }
                }
            }
            ans += acc;
        } else {
            // Every puzzle in this problem has each one unique answer.
            unreachable!();
        }
    }
    ans
}

fn parse_data(data: &str) -> Vec<Vec<usize>> {
    let re_data = Regex::new(r"^[0-9.]").unwrap();
    let re_sep = Regex::new(r"^-").unwrap();

    let mut ret: Vec<Vec<usize>> = Vec::new();
    let mut acc: Vec<String> = Vec::new();

    for line in data.lines() {
        if re_data.is_match(line) {
            acc.push(line.to_string());
        } else if re_sep.is_match(line) {
            continue;
        } else if !acc.is_empty() {
            ret.push(process_data(&acc));
            acc.clear();
        }
    }
    if !acc.is_empty() {
        ret.push(process_data(&acc));
    }
    ret
}

fn process_data(data: &[String]) -> Vec<usize> {
    let re_rplc1 = Regex::new(r"[^0-9.]").unwrap();
    let re_rplc2 = Regex::new(r"\.").unwrap();

    let mut tmp = data
        .iter()
        .fold(String::new(), |acc, x| format!("{}{}", acc, x));
    tmp = re_rplc1.replace_all(&tmp, "").to_string();
    tmp = re_rplc2.replace_all(&tmp, "0").to_string();
    tmp.chars().map(|c| c as usize - '0' as usize).collect()
}

fn check_data(pazzles: &[Vec<usize>]) -> Vec<Vec<u128>> {
    let mut ret: Vec<Vec<u128>> = Vec::new();

    'pazzle: for (seq, data) in pazzles.iter().enumerate() {
        if data.len() != NUM_OF_CELLS {
            println!(
                "[Warning] size of input data is mismatch [Grid: {}] (Ignored)\n{:?}",
                seq + 1,
                data
            );
            continue;
        }

        let mut grid: Vec<u128> = vec![0; 10];
        for (pos, &num) in data.iter().enumerate() {
            if num != 0 && grid[num] & ADJACENT_CELLS[pos] != 0 {
                println!(
                    "[Warning] there is a same number in the adjacent cells [Grid: {}] (Ignored)\n{:?}",
                    seq + 1,
                    data
                );
                continue 'pazzle;
            }
            grid[num] |= 1 << pos;
        }
        ret.push(grid);
    }
    ret
}

fn sudoku_solover(grid: &[u128]) -> Option<Vec<u128>> {
    let mut work_grid = grid.to_vec();

    // Start searching from cell #0.
    if _sudoku_solver(&mut work_grid, 0, 0b01) {
        Some(work_grid)
    } else {
        None
    }
}

fn _sudoku_solver(grid: &mut [u128], mut pos: usize, mut pos_bit: u128) -> bool {
    // Search for undetermined cell.
    // If we don't find it, we've found a solution.
    loop {
        // Found a solution
        if pos >= NUM_OF_CELLS {
            return true;
        }

        // Undetermined cell found
        if grid[0] & pos_bit != 0 {
            break;
        }
        pos_bit <<= 1;
        pos += 1;
    }

    // Try numbers 1 through 9.
    for n in 1..=9_usize {
        // If there is a same number in adjacent cells, try next number.
        if grid[n] & ADJACENT_CELLS[pos] != 0 {
            continue;
        }

        // Let number 'n' as temporary on the cell, and try next cell.
        grid[n] |= pos_bit;
        if _sudoku_solver(grid, pos + 1, pos_bit << 1) {
            return true;
        }
        // If the number isn't suitable, undo the status and try next number.
        grid[n] &= !pos_bit;
    }

    // Back to the previous cell if all numbers are not suitable.
    false
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0096() {
        assert_eq!(compute(super::FILE_DATA), 24702);
    }
}
