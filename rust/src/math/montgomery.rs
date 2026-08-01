// Montgomery modular multiplication

#[derive(Debug)]
pub struct MontgomeryMM {
    // R = 2^64

    // N  # odd number
    pub n: u64,

    // N⁻¹ (mod R)  [N * N⁻¹ ≡ 1 (mod R)]
    n_inv: u64,

    // R mod N  [= (R - N) mod N]
    pub r_mod_n: u64,

    // R² mod N
    r2_mod_n: u64,

    // (n >> 1) + 1  # This is used in div2()
    n_half: u64,

    // tmp_d = n - 1;
    // s = tmp_d.trailing_zeros();
    // d = tmp_d >> s
    pub d: u64,
    pub s: u32,

    // M(n - 1) = MR((n - 1)R²) = (n - 1)R  (mod n) = n - R mod n
    pub n_r: u64,
}

impl MontgomeryMM {
    pub fn new(n: u64) -> Self {
        // N must be an odd number
        debug_assert_eq!(n & 1, 1);

        let n_inv = Self::mod_r_inv(n);
        let r_mod_n = n.wrapping_neg() % n;
        let r2_mod_n = Self::rr_mod_n(n, n_inv, r_mod_n);

        let n_half = (n >> 1) + 1;

        let mut d = n - 1;
        let s = d.trailing_zeros();
        d >>= s;
        let n_r = n - r_mod_n;

        Self {
            n,
            n_inv,
            r_mod_n,
            r2_mod_n,
            n_half,
            d,
            s,
            n_r,
        }
    }

    // An Improved Integer Modular Multiplicative Inverse (modulo 2^w)
    // https://arxiv.org/abs/2204.04342
    //
    // n⁻¹ (mod R)  [R = 2^64]
    fn mod_r_inv(n: u64) -> u64 {
        let mut x = n.wrapping_mul(3) ^ 2;
        let mut y = 1_u64.wrapping_sub(n.wrapping_mul(x));

        x = x.wrapping_mul(y.wrapping_add(1));
        y = y.wrapping_mul(y);

        x = x.wrapping_mul(y.wrapping_add(1));
        y = y.wrapping_mul(y);

        x = x.wrapping_mul(y.wrapping_add(1));
        y = y.wrapping_mul(y);

        x.wrapping_mul(y.wrapping_add(1))
    }

    // a + b (mod n)
    #[inline(always)]
    fn _add(a: u64, b: u64, n: u64) -> u64 {
        debug_assert!(a < n);
        debug_assert!(b < n);

        let (x0, flag0) = a.overflowing_add(b);
        let (x1, flag1) = x0.overflowing_sub(n);

        if flag0 {
            x1
        } else {
            if flag1 { x0 } else { x1 }
        }
    }

    #[inline]
    pub fn add(&self, a: u64, b: u64) -> u64 {
        Self::_add(a, b, self.n)
    }

    // a - b (mod n)
    #[inline(always)]
    fn _sub(a: u64, b: u64, n: u64) -> u64 {
        debug_assert!(a < n);
        debug_assert!(b < n);

        let (x, flag) = a.overflowing_sub(b);

        if flag { x.wrapping_add(n) } else { x }
    }

    #[inline]
    pub fn sub(&self, a: u64, b: u64) -> u64 {
        Self::_sub(a, b, self.n)
    }

    // Montgomery Reduction
    // MR(x) = xR⁻¹ mod N  where 0 <= x < RN
    //
    // y <- floor(x/R) - floor((xN⁻¹ mod R)N / R)
    // MR(a) = if (y < 0) then { y + N } else { y }
    //
    // x - (xN⁻¹ mod R)N ≡ x - xN⁻¹N ≡ x - x ≡ 0  (mod R)
    //   --> R|(x - (xN⁻¹ mod R)N)
    // yR ≡ x - (xN⁻¹ mod R)N ≡ x  (mod N)
    //   --> y ≡ xR⁻¹  (mod N)
    // x < RN, (xN⁻¹ mod R)N < RN
    //   --> -N < y = floor(x/R) - floor((xN⁻¹ mod R)N / R) < N

    // MR(a)
    #[inline]
    pub fn mr64(&self, x: u64) -> u64 {
        debug_assert!(x < self.n);

        let (y, flag) = ((((x.wrapping_mul(self.n_inv) as u128) * (self.n as u128)) >> 64) as u64)
            .overflowing_neg();
        if flag { y.wrapping_add(self.n) } else { y }
    }

    #[inline(always)]
    fn _mr(x: u128, n: u64, n_inv: u64) -> u64 {
        let (y, flag) = ((x >> 64) as u64).overflowing_sub(
            ((((x as u64).wrapping_mul(n_inv) as u128) * (n as u128)) >> 64) as u64,
        );
        if flag { y.wrapping_add(n) } else { y }
    }

    #[inline]
    pub fn mr(&self, x: u128) -> u64 {
        Self::_mr(x, self.n, self.n_inv)
    }

    #[inline(always)]
    fn _mul_mr(a: u64, b: u64, n: u64, n_inv: u64) -> u64 {
        let ab = a as u128 * b as u128;
        Self::_mr(ab, n, n_inv)
    }

    #[inline]
    pub fn mul_mr(&self, a: u64, b: u64) -> u64 {
        debug_assert!(a < self.n);
        debug_assert!(b < self.n);

        Self::_mul_mr(a, b, self.n, self.n_inv)
    }

    #[inline]
    pub fn mul(&self, a: u64, b: u64) -> u64 {
        debug_assert!(a < self.n);
        debug_assert!(b < self.n);

        let ab = a as u128 * b as u128;
        self.mul_mr(self.mr(ab), self.r2_mod_n)
    }

    fn rr_mod_n(n: u64, n_inv: u64, r_mod_n: u64) -> u64 {
        let mut r2_mod = r_mod_n;

        // R -> 2R -> 4R (= 2^2 * R)
        for _ in 0..2 {
            r2_mod = Self::_add(r2_mod, r2_mod, n);
        }
        // MR(2^k*R * 2^k*R) = MR(2^2k * R²) = (2^2k * R²)R⁻¹  (mod N) = 2^2k * R  (mod N)
        //
        // (2^2)R -> (2^4)R -> (2^8)R -> (2^16)R -> (2^32)R -> (2^64)R = RR
        for _ in 0..5 {
            r2_mod = Self::_mul_mr(r2_mod, r2_mod, n, n_inv);
        }

        r2_mod
    }

    // M(x) = MR(xR²) = (xR²)R⁻¹ mod N = xR mod N
    #[inline]
    pub fn m(&self, x: u64) -> u64 {
        debug_assert!(x < self.n);

        self.mul_mr(x, self.r2_mod_n)
    }

    #[inline]
    pub fn div2(&self, a: u64) -> u64 {
        debug_assert!(a < self.n);

        if (a & 1) == 0 {
            a >> 1
        } else {
            (a >> 1) + self.n_half
        }
    }

    #[inline]
    pub fn powmod_mr(&self, mut xr: u64, mut exp: u64) -> u64 {
        debug_assert!(xr < self.n);

        let e = self.r_mod_n;
        let mut result = if (exp & 1) == 0 { e } else { xr };
        exp >>= 1;

        while exp > 0 {
            xr = self.mul_mr(xr, xr);
            result = self.mul_mr(result, if (exp & 1) == 0 { e } else { xr });
            exp >>= 1;
        }

        result
    }

    /*
    #[inline]
    pub fn powmod(&self, x: u64, mut exp: u64) -> u64 {
        let mut xr = self.m(x);
        let mut result = 1_u64;

        while exp > 0 {
            if (exp & 1) != 0 {
                result = self.mul_mr(result, xr);
            }

            xr = self.mul_mr(xr, xr);
            exp >>= 1;
        }

        result
    }
    */

    #[inline(always)]
    pub fn int_to_uint(&self, x: i64) -> u64 {
        if x >= 0 {
            x.unsigned_abs() % self.n
        } else {
            self.n - x.unsigned_abs() % self.n
        }
    }
}
