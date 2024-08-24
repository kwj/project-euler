// project euler: problem 1

export const compute = (limit: number): string => {
  const sumMultiples = (n: number): number => {
    const tmp = Math.trunc((limit - 1) / n);
    return Math.trunc((1 + tmp) * tmp * n / 2);
  };

  return String(sumMultiples(3) + sumMultiples(5) - sumMultiples(15));
};

export const solve = (): string => compute(1_000);
