package p0080

/*
when n < 100,

  i <= 10^99 * sqrt(n) < i + 1
 -->
  i^2 <= 10^198 * n < (i + 1)^2

  'i' is the 100-digit number we want.
*/

import (
	"math/big"
	"strconv"

	"pe-solver/internal/mylib"
)

func compute(limit, digit int) string {
	powerOf10 := new(big.Int).Exp(big.NewInt(10), big.NewInt(int64((digit-1)*2)), nil)

	var result int
	for n := 1; n <= limit; n++ {
		if sqrt_n := mylib.Isqrt(n); sqrt_n*sqrt_n == n {
			continue
		}
		s := new(big.Int).Sqrt(new(big.Int).Mul(powerOf10, big.NewInt(int64(n)))).String()[:digit]
		for _, rune := range s {
			result += int(rune) - int('0')
		}
	}

	return strconv.FormatInt(int64(result), 10)
}

func Solve() string {
	return compute(100, 100)
}
