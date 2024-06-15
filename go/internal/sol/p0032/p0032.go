package p0032

/*
m * n = mn (multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital)

- numbers of digits of multiplicand/multiplier must be 4 or less.
- if number of digits of multiplicand is 4, number of digits of multiplier is 1.
- if number of digits of multiplicand is 3, number of digits of multiplier is 2.

multiplicand/multiplier/product : 4-digits/1-digit/4-digits or 3-digits/2-digits/4-digits
*/

import (
	"strconv"

	"pe-solver/internal/mylib"
)

type pair struct {
	n    int
	prod int
}

func makeCands() []pair {
	result := make([]pair, 0)
	for m1 := 1_000; m1 < 10_000; m1++ {
		for m2 := 2; m2 < 10; m2++ {
			if m1*m2 >= 10_000 {
				continue
			}
			result = append(
				result,
				pair{m1*100_000 + m2*10_000 + m1*m2, m1 * m2},
			)
		}
	}

	for m1 := 100; m1 < 1_000; m1++ {
		for m2 := 10; m2 < 100; m2++ {
			if m1*m2 >= 10_000 {
				continue
			}
			result = append(
				result,
				pair{m1*1_000_000 + m2*10_000 + m1*m2, m1 * m2},
			)
		}
	}

	return result
}

func compute() string {
	type Set map[int]struct{}

	nSet := Set{}
	for _, tpl := range makeCands() {
		if mylib.IsPandigitalNZ(tpl.n) {
			nSet[tpl.prod] = struct{}{}
		}
	}

	var result int
	for v := range nSet {
		result += v
	}

	return strconv.FormatInt(int64(result), 10)
}

func Solve() string {
	return compute()
}
