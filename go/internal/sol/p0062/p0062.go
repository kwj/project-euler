package p0062

import (
	"fmt"
	"slices"

	"pe-solver/internal/mylib"
)

func compute(size int) string {
	key := func(n int) string {
		tmp := []rune(fmt.Sprintf("%d", n))
		slices.Sort(tmp)

		return string(tmp)
	}

	tbl := map[string][]int{}
	for n := 1; ; n++ {
		id := key(n * n * n)
		if v, ok := tbl[id]; ok {
			tbl[id] = append(v, n)
			if len(tbl[id]) == size {
				return fmt.Sprintf("%d", mylib.Pow(tbl[id][0], 3))
			}
		} else {
			tbl[id] = []int{n}
		}
	}
}

func Solve() string {
	return compute(5)
}
