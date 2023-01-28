
// tiny rational class

export interface Rational {
  readonly num: bigint;
  readonly denom: bigint;

  add(rat: Rat): Rat;
  sub(rat: Rat): Rat;
  mul(rat: Rat): Rat;
  div(rat: Rat): Rat;

  isInteger(): boolean;

}

type RatNumber = number | bigint | string;

function gcd(a: bigint, b: bigint): bigint {
  while (b !== 0n) {
    [a, b] = [b, a % b];
  }

  return a > 0 ? a : -a;
}

class Rat implements Rational {
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

  add(rat: Rat): Rat {
    return new Rat(
      this.num * rat.denom + rat.num * this.denom,
      this.denom * rat.denom
    );
  }

  sub(rat: Rat): Rat {
    return new Rat(
      this.num * rat.denom - rat.num * this.denom,
      this.denom * rat.denom
    );
  }

  mul(rat: Rat): Rat {
    return new Rat(
      this.num * rat.num,
      this.denom * rat.denom
    );
  }

  div(rat: Rat): Rat {
    if (rat.num === 0n) {
      throw new Error("division by zero");
    }
    return new Rat(
      this.num * rat.denom,
      this.denom * rat.num
    );
  }

  isInteger(): boolean {
    return this.denom === 1n;
  }
}

export function rational(n: RatNumber, d?: RatNumber): Rat {
  d = (d === undefined) ? 1 : d;
  return new Rat(n, d);
}
