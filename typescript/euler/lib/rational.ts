// tiny rational class

interface Rational {
  readonly num: bigint;
  readonly denom: bigint;

  add(rat: Ratio): Ratio;
  sub(rat: Ratio): Ratio;
  mul(rat: Ratio): Ratio;
  div(rat: Ratio): Ratio;
  negate(): Ratio;

  isInteger(): boolean;
  isZero(): boolean;
}

type RatNumber = number | bigint | string;

const gcd = (a: bigint, b: bigint): bigint => {
  while (b !== 0n) {
    [a, b] = [b, a % b];
  }

  return a > 0 ? a : -a;
};

export class Ratio implements Rational {
  readonly num: bigint;
  readonly denom: bigint;

  constructor(num: RatNumber, denom: RatNumber) {
    const n = BigInt(num), d = BigInt(denom);
    const tmp = gcd(n, d);
    const sign = (d < 0) ? -1n : 1n;

    this.num = sign * (n / tmp);
    this.denom = sign * (d / tmp);

    Object.freeze(this);
  }

  add(rat: Ratio): Ratio {
    return new Ratio(
      this.num * rat.denom + rat.num * this.denom,
      this.denom * rat.denom,
    );
  }

  sub(rat: Ratio): Ratio {
    return new Ratio(
      this.num * rat.denom - rat.num * this.denom,
      this.denom * rat.denom,
    );
  }

  mul(rat: Ratio): Ratio {
    return new Ratio(
      this.num * rat.num,
      this.denom * rat.denom,
    );
  }

  div(rat: Ratio): Ratio {
    if (rat.num === 0n) {
      throw new Error("division by zero");
    }
    return new Ratio(
      this.num * rat.denom,
      this.denom * rat.num,
    );
  }

  negate(): Ratio {
    return new Ratio(
      -this.num,
      this.denom,
    );
  }

  isInteger(): boolean {
    return this.denom === 1n;
  }

  isZero(): boolean {
    return this.num === 0n;
  }
}

export const rational = (n: RatNumber, d?: RatNumber): Ratio => {
  d = (d === undefined) ? 1 : d;
  return new Ratio(n, d);
};
