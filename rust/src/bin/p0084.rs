// Project Euler: Problem 84

/*
  Stochastic matrix: stoch_matrix

   Current: S0       S1       S2
        +--------+--------+--------+ State:
  Next  |        |        |        |  S0: no doubles
   S0   |  s00   |  s10   |  s20   |  S1: one doubles occurred
        |        |        |        |  S2: two consecutive dobules occurred
        +--------+--------+--------+
  Next  |        |        |        | s00: transition probability if no doubles occurred (the next state is S0)
   S1   |  s01   | s11=0  | s21=0  | s11: transition probability if fist doubles occurred (the next state is S1)
        |        |        |        |
        +--------+--------+--------+ s10: transition probability if no doubles occurred (the next state is S0)
  Next  |        |        |        | s12: transition probability if two consecutive doubles occurred (the next state is S2)
   S2   | s02=0  |  s12   | s22=0  |
        |        |        |        | s20: transition probability if both doubles and no doubles occurred (the next state is S0)
        +--------+--------+--------+      Note: go to JAIL@S0 if three consecutive doubles occurred

  s##:
         | GO  A1  CC1 ... [current square]
    -----+---------------------------
     GO  | ##  ##  ##  ...
     A1  | ##  ##  ##  ...
     CC1 | ##  ##  ##  ...
      .  |  .   .   .
  [next square]
*/

const GO: usize = 0;
const CC1: usize = 2;
const R1: usize = 5;
const CH1: usize = 7;
const JAIL: usize = 10;
const C1: usize = 11;
const U1: usize = 12;
const R2: usize = 15;
const CC2: usize = 17;
const CH2: usize = 22;
const E3: usize = 24;
const R3: usize = 25;
const U2: usize = 28;
const G2J: usize = 30;
const CC3: usize = 33;
const CH3: usize = 36;
const H2: usize = 39;

euler::run_solver!(84);

fn solve() -> String {
    compute(4, 3)
}

fn compute(nfaces: usize, nsquares: usize) -> String {
    let mut dice_prblty_without_dbl = vec![0.0; nfaces * 2 + 1];
    let mut dice_prblty_dbl = vec![0.0; nfaces * 2 + 1];

    for n1 in 1..=nfaces {
        for n2 in 1..=nfaces {
            if n1 != n2 {
                dice_prblty_without_dbl[n1 + n2] += 1.0 / (nfaces * nfaces) as f64;
            } else {
                dice_prblty_dbl[n1 + n2] = 1.0 / (nfaces * nfaces) as f64;
            }
        }
    }

    let mut stoch_matrix: Vec<Vec<f64>> = (0..120).map(|_| vec![0.0; 120]).collect();

    // no doubles occurred
    // setup 's00', 's10' and 's20'
    for x in 0..120_usize {
        for v in 2..=(nfaces * 2) {
            stoch_matrix[(x + v) % 40][x] = dice_prblty_without_dbl[v];
        }
    }

    // doubles occurrd
    // setup 's01'
    for x in 0..40_usize {
        for v in 2..=(nfaces * 2) {
            stoch_matrix[(x + v) % 40 + 40][x] = dice_prblty_dbl[v];
        }
    }
    // setup 's12'
    for x in 40..80_usize {
        for v in 2..=(nfaces * 2) {
            stoch_matrix[(x + v) % 40 + 80][x] = dice_prblty_dbl[v];
        }
    }
    // modify 's20'
    for x in 80..120_usize {
        stoch_matrix[JAIL][x] += dice_prblty_dbl.iter().sum::<f64>();
    }

    // Go to Jail
    for offset in (0..=80_usize).step_by(40) {
        for x in 0..120_usize {
            stoch_matrix[JAIL][x] += stoch_matrix[G2J + offset][x];
            stoch_matrix[G2J + offset][x] = 0.0;
        }
    }

    // Chance Card
    // note: It must be processed before Communy Chest because the CH3 -> CC3 path is exist.
    for offset in (0..=80_usize).step_by(40) {
        for chance in [CH1, CH2, CH3] {
            for (current, prblty) in stoch_matrix[chance + offset].clone().iter().enumerate() {
                let next_r = match chance {
                    CH1 => R2,
                    CH2 => R3,
                    _ => R1, // CH3 => R1
                };
                let next_u = if chance == CH2 { U2 } else { U1 };
                for next_sq in [
                    GO,
                    C1,
                    E3,
                    H2,
                    R1,
                    next_r,
                    next_r,
                    next_u,
                    (chance - 3) % 40,
                ] {
                    stoch_matrix[next_sq + offset][current] += *prblty / 16.0;
                }
                stoch_matrix[JAIL][current] += *prblty / 16.0;

                stoch_matrix[chance][current] -= (*prblty / 16.0) * 10.0;
            }
        }
    }

    // Community Chest
    for offset in (0..=80_usize).step_by(40) {
        for chest in [CC1, CC2, CC3] {
            for x in 0..120_usize {
                stoch_matrix[GO + offset][x] += stoch_matrix[chest + offset][x] / 16.0;
                stoch_matrix[JAIL][x] += stoch_matrix[chest + offset][x] / 16.0;
                stoch_matrix[chest + offset][x] -= stoch_matrix[chest + offset][x] / 8.0;
            }
        }
    }

    let steady_state = go_steady(&stoch_matrix);

    let mut result = (0..40)
        .map(|i| {
            (
                format!("{:02}", i),
                steady_state[i][0] + steady_state[40 + i][0] + steady_state[80 + i][0],
            )
        })
        .collect::<Vec<(String, f64)>>();
    result.sort_by(|a, b| (b.1).partial_cmp(&a.1).unwrap());
    result
        .into_iter()
        .take(nsquares)
        .fold(String::new(), |acc, x| format!("{}{}", acc, x.0))
}

fn go_steady(matrix: &[Vec<f64>]) -> Vec<Vec<f64>> {
    assert!(matrix.len() == matrix[0].len(), "It's not a square matrix");

    let size = matrix.len();
    let mut work = matrix
        .iter()
        .map(|v| (*v).clone())
        .collect::<Vec<Vec<f64>>>();

    // To converge quickly, use property of exponentiation.
    //   M^{n+n} = M^{n} * M^{n}
    loop {
        let prev = work.clone();
        for x in 0..size {
            for y in 0..size {
                work[x][y] = (0..size).map(|i| prev[x][i] * prev[i][y]).sum();
            }
        }
        if is_steady(&work[0], &prev[0]) {
            break;
        }
    }
    work
}

fn is_steady(x: &[f64], y: &[f64]) -> bool {
    assert!(x.len() == y.len(), "mismatch!");
    x.iter()
        .zip(y.iter())
        .all(|(a, b)| (a - b).abs() < f64::EPSILON)
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0084_4() {
        assert_eq!(compute(4, 3), "101524");
    }
    #[test]
    fn p0084_6() {
        assert_eq!(compute(6, 3), "102400");
    }
}
