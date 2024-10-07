// Project Euler: Problem 78

/*
 p(5) = 7
 p(10) = 42
 p(50) = 204226
 p(100) = 190569292
 p(200) = 3972999029388
 p(500) = 2300165032574323995027
 p(1000) = 24061467864032622473692149727991
   ...

 I needed to find another way instead of dynamic programming.
 Unfortunately, I gave up trying to solve it on my own at last.

 I saw following pages.

   https://en.wikipedia.org/wiki/Partition_(number_theory)
   https://en.wikipedia.org/wiki/Partition_function_(number_theory)
   https://en.wikipedia.org/wiki/Pentagonal_number_theorem

   p(n) = Sigma{k ∈ Z/{0}} (-1)^(k+1) * p(n - k(3k-1)/2)
        = p(n-1) + p(n-2) - p(n-5) - p(n-7) + p(n-12) + p(n-15) - p(n-22) - ...

     [p(0) = 1, p(k) = 0 when k < 0]
*/

euler::run_solver!(78);

fn solve() -> String {
    compute(1_000_000).to_string()
}

fn compute(denom: i64) -> usize {
    // generalized pentagonal numbers: gp[0] = 1, gp[1] = 2, gp[2] = 5, gp[3] = 7, ...
    let mut gp: Vec<usize> = Vec::new();
    let mut gpnum_gen = GPnumbers {
        acc: 0,
        gap: 1,
        step: 1,
        flag: 0,
    };
    gp.push(gpnum_gen.next().unwrap());

    // number of partitions of n: p[n]
    let mut p: Vec<i64> = vec![1];

    let mut n: usize = 1;
    loop {
        if n > *gp.last().unwrap() {
            gp.push(gpnum_gen.next().unwrap());
        }

        let mut rem: i64 = 0;
        for (i, x) in gp.iter().enumerate() {
            if *x > n {
                break;
            }
            if i % 4 < 2 {
                rem += p[n - *x];
            } else {
                rem -= p[n - *x];
            }
        }

        rem %= denom;
        if rem == 0 {
            break;
        }
        p.push(rem);
        n += 1;
    }

    n
}

// Generalized pentagonal numbers
//       0   1   2   5   7   12   15   22   26   35   40   51   57   70   77   92   100   117  ...
// diff:    1   1   3   2   5    3    7    4    9    5    11   6    13   7    15    8    17
// g/s      g   s   g   s   g    s    g    s    g    s     g   s     g   s     g    s     g
//   [g: gap, s: step]
#[derive(Debug)]
struct GPnumbers {
    acc: usize,
    gap: usize,
    step: usize,
    flag: usize,
}

impl Iterator for GPnumbers {
    type Item = usize;

    fn next(&mut self) -> Option<usize> {
        if self.flag == 0 {
            self.acc += self.gap;
            self.gap += 2;
        } else {
            self.acc += self.step;
            self.step += 1;
        }

        self.flag = (self.flag + 1) % 2;
        Some(self.acc)
    }
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0078() {
        assert_eq!(compute(1_000_000), 55374);
    }
}
