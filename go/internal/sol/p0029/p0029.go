package p0029

import (
	"slices"
	"strconv"

	"pe-solver/internal/mylib"
)

func dupTbl(upper int) []int {
	sum := func(ns []int) int {
		var result int
		for v := range slices.Values(ns) {
			result += v
		}

		return result
	}

	maxExp := mylib.MaxExp(upper, 2)
	tbl := make([]int, maxExp+1)

	for x := 2; x <= maxExp; x++ {
		dups := make([]int, upper+1)
		for y := 1; y < x; y++ {
			k := mylib.Lcm(x, y) / x
			for i := max(k, 2); i <= (upper*y)/x; i += k {
				dups[i] = 1
			}
		}
		tbl[x] = sum(dups)
	}

	return tbl
}

func compute(upper int) string {
	tbl := dupTbl(upper)
	baseLimit := mylib.Isqrt(upper)
	skipFlag := make([]bool, baseLimit+1)

	result := mylib.Pow((upper - 1), 2)
	for b := 2; b <= baseLimit; b++ {
		if skipFlag[b] {
			continue
		}
		for e := 2; e <= mylib.MaxExp(upper, b); e++ {
			result -= tbl[e]
			if tmp := mylib.Pow(b, e); tmp <= baseLimit {
				skipFlag[tmp] = true
			}
		}
	}
	return strconv.FormatInt(int64(result), 10)
}

func Solve() string {
	return compute(100)
}
