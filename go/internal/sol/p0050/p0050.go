package p0050

import (
	"slices"
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
	csumGen := new(csumGenerator)
	lst := initCsumLst(csumGen, limit)

	result := 0
	left := 0
	k := 0
	for lst[left+k]-lst[left] < limit {
		for idx, v := range slices.Backward(lst) {
			if idx <= left+k {
				break
			}

			diff := v - lst[left]
			if diff >= limit {
				continue
			}
			if mylib.IsPrime(diff) {
				k = idx - left
				result = diff
				break
			}
		}

		lst = append(lst, csumGen.next())
		left++
	}

	return strconv.FormatInt(int64(result), 10)
}

func Solve() string {
	return compute(1_000_000)
}
