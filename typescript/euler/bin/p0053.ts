// project euler: problem 53

export const compute = (num: number, boundary: number): string => {
  let n = num, x = num;
  let c = 1, r = 1;
  let answer = boundary > 0 ? 0 : num * 2;

  while (r <= (n >> 1)) {
    c = c * x / r;
    if (c > boundary) {
      answer += n - (r * 2) + 1;
      c = c * r / n;
      n -= 1;
    } else {
      r += 1;
    }
    x -= 1;
  }

  return String(answer);
};

export const solve = (): string => compute(100, 1_000_000);
