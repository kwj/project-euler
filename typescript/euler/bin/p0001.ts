// project euler: problem 1

const sumMultiple = (n: number, limit: number): number => {
  const trunc = Math.trunc;
  const upper = limit - 1;

  return trunc((n + (upper - (upper % n))) * trunc(upper / n) / 2);
};

export const compute = (limit: number): string =>
  String(
    sumMultiple(3, limit) + sumMultiple(5, limit) - sumMultiple(15, limit),
  );

export const solve = (): string => compute(1_000);
