package p0046

import (
	"strconv"

	"pe-solver/internal/mylib"
)

// 'n' must be a even number
func isTwiceSquare(n int) bool {
	tmp := mylib.Isqrt(n / 2)

	return tmp*tmp == n/2
}

func compute() string {
	// odd prime numbers under nine which is the first odd composite number
	oddPrimes := []int{3, 5, 7}

loop:
	for x := 9; ; x += 2 {
		if mylib.IsPrime(x) {
			oddPrimes = append(oddPrimes, x)
			continue
		}

		for _, p := range oddPrimes {
			if isTwiceSquare(x - p) {
				continue loop
			}
		}

		return strconv.FormatInt(int64(x), 10)
	}
}

func Solve() string {
	return compute()
}
