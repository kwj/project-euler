package p0077

import (
	"slices"
	"strconv"

	"pe-solver/internal/mylib"
)

func compute(boundary int) string {
	pGen := mylib.PrimeGenerator{}
	var primes []int

	for {
		primes = append(primes, pGen.Next())
		tbl := make([]int, len(primes)+1)
		tbl[0] = 1
		for p := range slices.Values(primes) {
			for j := p; j < len(tbl); j++ {
				tbl[j] += tbl[j-p]
			}
		}

		// If 'i' is a prime, tbl[i] is one grater than the number of sum of prime combinations.
		//
		// example:
		// i=7, tbl[7]=3  (2+2+3, 2+5, 7)
		// i=8, tbl[8]=3  (2+2+2+2, 2+3+3, 3+5)
		//
		// Eight is the first number that can be written as different three ways of
		// sum of primes, NOT seven.
		var adjustment int
		if mylib.IsPrime(len(tbl) - 1) {
			adjustment = 1
		}
		if tbl[len(tbl)-1] > boundary+adjustment {
			return strconv.FormatInt(int64(len(tbl)-1), 10)
		}
	}
}

func Solve() string {
	return compute(5_000)
}
