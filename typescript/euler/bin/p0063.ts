// project euler: problem 63

/*
  n - 1 <= log10(m^n) < n    [m>0, n>0]
    --> n - 1 <= n * log10(m) < n
    --> m < 10
   and
    --> (n - 1)/n <= log10(m)
*/

export const compute = (): string => {
  let acc = 0;
  let cnt = 0;
  let m = 1;
  let n = 1;
  while (m < 10) {
    const upper_m = Math.log10(m);
    while ((n - 1) / n <= upper_m) {
      n += 1;
      cnt += 1;
    }
    m += 1;
    acc += cnt;
  }

  return String(acc);
};

export const solve = (): string => compute();
