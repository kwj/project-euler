package p0049

import (
	"fmt"
	"slices"
	"strconv"

	"pe-solver/internal/mylib"
)

func makePrimeTbl(ndigits int) map[string][]int {
	tbl := make(map[string][]int)

	for _, p := range mylib.Primes(mylib.Pow(10, ndigits-1), mylib.Pow(10, ndigits)) {
		tmp := []rune(strconv.FormatInt(int64(p), 10))
		slices.Sort(tmp)
		key := string(tmp)
		if v := tbl[key]; v != nil {
			tbl[key] = append(v, p)
		} else {
			tbl[key] = []int{p}
		}
	}

	return tbl
}

func compute() string {
	// 4-digit prime numbers
	tbl := makePrimeTbl(4)

	for _, lst := range tbl {
		if len(lst) < 3 {
			continue
		}
		for i := 0; i < len(lst)-2; i++ {
			for j := i + 1; j <= len(lst)-1; j++ {
				tmp := 2*lst[j] - lst[i] // 2(x + a) - x = x + 2a
				if slices.Contains(lst, tmp) && lst[i] != 1487 && lst[j] != 4817 {
					return fmt.Sprintf("%d%d%d", lst[i], lst[j], tmp)
				}
			}
		}
	}

	panic("unreachable")
}

func Solve() string {
	return compute()
}
