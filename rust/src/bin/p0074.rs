// Project Euler: Problem 74

euler::run_solver!(74);

fn solve() -> String {
    compute().to_string()
}

fn compute() -> i64 {
    let limit: usize = 1_000_000;
    let thr: usize = 60;
    let mut chain_tbl: Vec<usize> = vec![0; limit + 1];
    let mut cnt: i64 = 0;

    for n in 1..limit {
        let mut footprints: Vec<usize> = Vec::new();
        let mut steps: usize = 0;
        let mut pos = n;
        while !footprints.contains(&pos) {
            if pos < limit && chain_tbl[pos] != 0 {
                steps += chain_tbl[pos];
                break;
            }
            footprints.push(pos);
            pos = fact_sum(pos);
            steps += 1;
        }

        if steps - footprints.len() < thr && thr <= steps {
            cnt += 1;
        }
        for pos in footprints {
            if pos < limit {
                chain_tbl[pos] = steps;
            }
            steps -= 1;
        }
    }

    cnt
}

fn fact_sum(mut n: usize) -> usize {
    let tbl: [usize; 10] = [1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880];
    if n == 0 {
        return tbl[0];
    }

    let mut acc: usize = 0;
    while n > 0 {
        acc += tbl[n % 10];
        n /= 10;
    }

    acc
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0074() {
        assert_eq!(compute(), 402);
    }
}
