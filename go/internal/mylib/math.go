package mylib

import (
	"fmt"
	"math/big"
	"math/bits"
)

// This code works well only on 32 or 64-bit environments.
//
//	32 (for 32-bit environment)
//	64 (for 64-bit environment)
const intSize = 32 << (^uint(0) >> 63)

func Abs(x int) int {
	// -1 (when x is negative), 0 (otherwise)
	sign := x >> (intSize - 1)

	// using two's complement
	return (x ^ sign) - sign
}

func Gcd(a, b int) int {
	x, y := uint(Abs(a)), uint(Abs(b))
	if x == 0 {
		return int(y)
	}
	if y == 0 {
		return int(x)
	}

	ntz_x := bits.TrailingZeros(x)
	ntz_y := bits.TrailingZeros(y)
	x >>= ntz_x
	y >>= ntz_y

	for {
		if x >= y {
			x -= y
			if x == 0 {
				return int(y << min(ntz_x, ntz_y))
			}
		} else {
			x, y = y-x, x
		}

		x >>= bits.TrailingZeros(x)
	}
}

func Lcm(a, b int) int {
	result := uint((Abs(a) / Gcd(a, b)) * Abs(b))
	if result&(1<<(intSize-1)) != 0 {
		panic("the result is overflow")
	}

	return int(result)
}

// If both arguments are zero, the returned value is (0, 0, 0).
// Otherwise, the returned value is (gcd(a, b), x, y) such that x and y are integers, and ax + by = gcd(a, b).
// Note: 'x' and 'y' are a pair of Bézout's coefficients for (a, b).
func ExtGcd(a, b int) (int, int, int) {
	sgn := func(x int) int {
		if x < 0 {
			return -1
		} else {
			return 1
		}
	}

	if a == 0 && b == 0 {
		return 0, 0, 0
	}

	gcd, x, y := auxExtGcd(int(Abs(a)), int(Abs(b)))
	return gcd, x * sgn(a), y * sgn(b)
}

func auxExtGcd(a, b int) (int, int, int) {
	if b == 0 {
		return int(Abs(a)), 1, 0
	}

	gcd, s, t := auxExtGcd(b, a%b)
	return gcd, t, s - a/b*t
}

func Pow(b, e int) int {
	result := 1
	for e > 0 {
		if e&1 != 0 {
			result *= b
		}
		e >>= 1
		b *= b
	}
	return result
}

func InvMod(a, m int) (int, error) {
	if m == 0 {
		return 0, fmt.Errorf("modulo must no be zero")
	}
	gcd, result, _ := ExtGcd(a, m)
	if gcd != 1 {
		return gcd, fmt.Errorf("there is no modular multiplicative inverse because GCD(%d, %d) = %d != 1", a, m, gcd)
	}

	return result, nil
}

func PowerMod(b, e, m int) int {
	if m == 0 {
		panic("modulo must not be zero")
	}

	if e < 0 {
		inv_mod, err := InvMod(b, m)
		if err != nil {
			panic("base is not invertible for the given modulus")
		}
		return auxPowerMod(inv_mod, -e, m)
	} else {
		return auxPowerMod(b, e, m)
	}
}

func auxPowerMod(b, e, m int) int {
	base := big.NewInt(int64(b))
	exp := big.NewInt(int64(e))
	modulus := big.NewInt(int64(m))

	result := new(big.Int)
	result.Exp(base, exp, modulus)

	return int(result.Int64())
}

func isqrtAux(c, n int) int {
	if c == 0 {
		return 1
	} else {
		k := (c - 1) >> 1
		a := isqrtAux(c>>1, n>>(2*k+2))
		return (a << k) + (n>>(k+2))/a
	}
}

func Isqrt(n int) int {
	if n < 0 {
		panic("argument must not be negative")
	}
	if n == 0 {
		return 0
	}

	if a := isqrtAux((bits.Len(uint(n))-1)>>1, n); n < a*a {
		return a - 1
	} else {
		return a
	}

}

// Returns the largest exponent 'e' for which pow(base, e) does not exceed 'n'.
func MaxExp(n, base int) int {
	e := 0
	for n >= base {
		n /= base
		e += 1
	}

	return e
}

func Binomial(n, k int) int {
	// C(n,k) = C(n,n-k)
	if k > n/2 {
		k = n - k
	}

	result := 1
	for i := 1; i <= k; i++ {
		result = (result * (n - k + i)) / i
	}

	return result
}

func Factorial(n int) int {
	if n < 0 {
		panic("argument must not be negative")
	}

	result := 1
	for ; n > 1; n-- {
		result *= n
	}

	return result
}

func PowBig(base *big.Int, e int) *big.Int {
	exp := big.NewInt(int64(e))
	result := new(big.Int)

	return result.Exp(base, exp, nil)
}

func IsTriangular(n int) bool {
	tmp := 8*n + 1
	tmp_sqrt := Isqrt(tmp)

	return tmp_sqrt&1 == 1 && tmp_sqrt*tmp_sqrt == tmp
}

func IsSquare(n int) bool {
	n_sqrt := Isqrt(n)

	return n_sqrt*n_sqrt == n
}

func IsPentagonal(n int) bool {
	tmp := 24*n + 1
	tmp_sqrt := Isqrt(tmp)

	return tmp_sqrt%6 == 5 && tmp_sqrt*tmp_sqrt == tmp
}

func IsHexagonal(n int) bool {
	tmp := 8*n + 1
	tmp_sqrt := Isqrt(tmp)

	return tmp_sqrt%4 == 3 && tmp_sqrt*tmp_sqrt == tmp
}
