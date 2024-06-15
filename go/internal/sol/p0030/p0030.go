package p0030

import (
	"slices"
	"strconv"

	"pe-solver/internal/mylib"
)

func maxNdigits(e int) int {
	k := mylib.Pow(9, e)
	x := 2
	for x*k > mylib.Pow(10, x-1) {
		x++
	}
	return x - 1
}

/*
This implementation assumes that the order of output combinations
is followed the order of the input slices.

Otherwise, we need to use other method.
*/

func compute(e int) string {
	var digits = []int{0, 1, 2, 3, 4, 5, 6, 7, 8, 9}
	tbl := make([]int, 10)
	for x := range tbl {
		tbl[x] = mylib.Pow(x, e)
	}

	var result int
	for n := 2; n <= maxNdigits(e); n++ {
		ch := mylib.CombinationsWithRepetition(digits, n)
		for lst := range ch {
			acc := 0
			for _, x := range lst {
				acc += tbl[x]
			}
			tmp := mylib.Digits(acc)
			slices.Sort(tmp)
			if slices.Compare(tmp, lst) == 0 {
				result += acc
			}
		}
	}

	return strconv.FormatInt(int64(result), 10)
}

func Solve() string {
	return compute(5)
}
