package p0050

import (
	"strconv"

	"pe-solver/internal/mylib"
)

type csumGenerator struct {
	p    int
	csum int
}

func (cs *csumGenerator) next() int {
	cs.p = mylib.NextPrime(cs.p)
	cs.csum += cs.p

	return cs.csum
}

func initCsumLst(csumGen *csumGenerator, limit int) []int {
	lst := []int{0}
	for lst[len(lst)-1] < limit {
		lst = append(lst, csumGen.next())
	}

	return lst
}

func compute(limit int) string {
	if limit < 3 {
		panic("limit must be larger than 2")
	}

	csumGen := new(csumGenerator)
	lst := initCsumLst(csumGen, limit)

	k := len(lst) - 2
	left := 0
	for {
		diff := lst[left+k] - lst[left]
		if diff >= limit {
			left = 0
			k -= 1
		} else if !mylib.IsPrime(diff) {
			left += 1
			if left+k >= len(lst) {
				lst = append(lst, csumGen.next())
			}
		} else {
			return strconv.FormatInt(int64(diff), 10)
		}
	}

	panic("unreachable")
}

func Solve() string {
	return compute(1_000_000)
}
