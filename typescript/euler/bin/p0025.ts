// project euler: problem 25

export const compute = (digit: number): string => {
  const limit = 10n ** (BigInt(digit) - 1n);
  let nth = 2;
  let fib1 = 1n, fib2 = 1n;
  while (fib2 < limit) {
    nth += 1;
    [fib1, fib2] = [fib2, fib1 + fib2];
  }

  return String(nth);
};

export const solve = (): string => compute(1_000);
