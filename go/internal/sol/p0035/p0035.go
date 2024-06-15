package p0035

import (
	"strconv"

	"pe-solver/internal/mylib"
)

func isCircularPrime(n int) bool {
	// 'n' must be a prime number.
	k := mylib.NumOfDigits(n, 10) - 1
	d := mylib.Pow(10, k)

	for k > 0 {
		if n = (n%10)*d + n/10; !mylib.IsPrime(n) {
			return false
		}
		k--
	}

	return true
}

func compute(limit int) string {
	var result int
	for _, prime := range mylib.Primes(limit) {
		if isCircularPrime(prime) {
			result++
		}
	}

	return strconv.FormatInt(int64(result), 10)
}

func Solve() string {
	return compute(1_000_000)
}
