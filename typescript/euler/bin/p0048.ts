// project euler: problem 48

export const compute = (exp: bigint): string => {
  const modulus = 10n ** 10n;

  let acc = 0n;
  for (let x = 1n; x <= exp; x++) {
    if (x % 10n !== 0n) {
      acc += (x ** x) % modulus;
    }
  }
  acc = acc % modulus;

  return acc.toString().padStart(10, "0");
};

export const solve = (): string => compute(1_000n);
