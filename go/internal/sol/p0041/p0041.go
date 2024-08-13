package p0041

/*
(sum_{1}_{9} n) mod 3 = 45 mod 3 = 0  --> 9-digit pandigital number is a multiple of 3.
(sum_{1}_{8} n) mod 3 = 36 mod 3 = 0  --> 8-digit pandigital number is a multiple of 3.
(sum_{1}_{7} n) mod 3 = 28 mod 3 = 1
(sum_{1}_{6} n) mod 3 = 21 mod 3 = 0  --> 6-digit pandigital number is a multiple of 3.
(sum_{1}_{5} n) mod 3 = 15 mod 3 = 0  --> 5-digit pandigital number is a multiple of 3.
(sum_{1}_{4} n) mod 3 = 10 mod 3 = 1

From the problem statement, 2143 is a 4-digit pandigital and is also prime.
So, we start to search in descending order.

This implementation assumes that the order of output permutations
is followed the order of the input slices.
Otherwise, we need to use other method.
*/

import (
	"slices"
	"strconv"

	"pe-solver/internal/mylib"
)

func compute() string {
	digitGroup := [][]int{
		{7, 6, 5, 4, 3, 2, 1},
		{4, 3, 2, 1},
	}

	for grp := range slices.Values(digitGroup) {
		for lst := range mylib.Permutations(grp, len(grp)) {
			slices.Reverse(lst)
			n := mylib.UnDigits(lst)
			if mylib.IsPrime(n) {
				return strconv.FormatInt(int64(n), 10)
			}
		}
	}

	panic("unreachable")
}

func Solve() string {
	return compute()
}
