package p0095

import (
	"slices"
	"strconv"

	"pe-solver/internal/mylib"
)

func updateTbl(tbl, indices []int, v int) {
	for _, i := range indices {
		tbl[i] = v
	}
}

func compute(limit int) string {
	nextPosTbl := mylib.SigmaTbl(1, limit)
	for i := 1; i < len(nextPosTbl); i++ {
		nextPosTbl[i] -= i
	}

	chainTbl := make([]int, limit+1)
	var maxLength int

	for n := 2; n <= limit; n++ {
		if chainTbl[n] != 0 {
			continue
		}

		pos := n
		chain := make([]int, 0)
		for chainTbl[pos] == 0 {
			chain = append(chain, pos)
			pos = nextPosTbl[pos]
			if pos <= 1 || pos > limit || slices.Contains(chain, pos) {
				break
			}
		}

		if pos <= 1 || pos > limit || chainTbl[pos] != 0 {
			updateTbl(chainTbl, chain, -1)
			continue
		}

		i := slices.Index(chain, pos)
		length := len(chain) - i + 1
		updateTbl(chainTbl, chain[:i], -1)
		updateTbl(chainTbl, chain[i:], length)
		maxLength = max(maxLength, length)
	}

	return strconv.FormatInt(int64(slices.Index(chainTbl, maxLength)), 10)
}

func Solve() string {
	return compute(1_000_000)
}
