
// project euler: problem 78

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

  I consider only value of 'mod 1_000_000' because the problem is divisible by one million or not.

  memo:
    # Generalized pentagonal numbers
    #        0   1   2   5   7   12   15   22   26   35   40   51   57   70   77   92   100   117  ...
    # diff:    1   1   3   2   5    3    7    4    9    5    11   6    13   7    15    8    17
    # g/s      g   s   g   s   g    s    g    s    g    s     g   s     g   s     g    s     g
    #   [g: gap, s: step]
*/

export function compute(denom: number): string {
  //number of partitions of n: p[n]
  const p: number[] = [1];

  let n = 0;
  let rem = 0;
  do {
    n += 1;
    rem = 0;
    let sign = 1;
    let k = 1;
    let n1 = 0;
    let n2 = 0;
    do {
      n1 = n - k * (3 * k - 1) / 2;
      n2 = n - k * (3 * k + 1) / 2;
      if (n1 >= 0) {
        rem = (rem + sign * p[n1]) % denom;
      }
      if (n2 >= 0) {
        rem = (rem + sign * p[n2]) % denom;
      }
      sign = -sign;
      k += 1;
    } while (n2 > 0 && n1 > 0)

    p.push(rem);
  } while (rem !== 0)

  return String(n);
}

export function solve(): void {
  const t0 = performance.now();
  const result = compute(1_000_000);
  const t1 = performance.now();
  const duration_ms = (t1 - t0).toFixed(4);

  console.log(`Answer: ${result}`);
  console.log(`Elapsed time: ${duration_ms} msec.`);

  return;
}
