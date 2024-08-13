package p0026

import (
	"maps"
	"slices"
	"strconv"

	"pe-solver/internal/mylib"
)

// This function is not the correct Carmichael function because
// the function assumes that the argument is not a multiple of 2.
func carmichael(n int) int {
	var result = 1
	for b, e := range maps.All(mylib.Frequencies(mylib.PrimeFactorization(n))) {
		result = mylib.Lcm(result, (b-1)*mylib.Pow(b, (e-1)))
	}

	return result
}

func pp(n int) int {
	for n%2 == 0 {
		n /= 2
	}
	for n%5 == 0 {
		n /= 5
	}

	return n
}

func findRepetendLength(d int) int {
	if d = pp(d); d == 1 {
		return 0
	}

	for k := range slices.Values(mylib.Divisors(carmichael(d))) {
		if mylib.PowerMod(10, k, d) == 1 {
			return k
		}
	}

	panic("unreachable")
}

func compute(limit int) string {
	var maxLength int
	var result int

	for i := limit - 1; i >= limit/2; i-- {
		if i <= maxLength {
			break
		}
		if repLength := findRepetendLength(i); repLength > maxLength {
			result = i
			maxLength = repLength
		}
	}

	return strconv.FormatInt(int64(result), 10)
}

func Solve() string {
	return compute(1_000)
}
