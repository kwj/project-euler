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
	csumGen := new(csumGenerator)
	lst := initCsumLst(csumGen, limit)

	result := 0
	startIdx := 0
	consecLength := 0
	for lst[startIdx+consecLength]-lst[startIdx] < limit {
		var idx int
		for idx = len(lst) - 1; idx > startIdx+consecLength; idx-- {
			diff := lst[idx] - lst[startIdx]
			if diff >= limit {
				continue
			}
			if mylib.IsPrime(diff) {
				break
			}
		}

		if idx != startIdx+consecLength {
			consecLength += idx - (startIdx + consecLength) - 1
			result = lst[idx] - lst[startIdx]
		}
		lst = append(lst, csumGen.next())
		startIdx++
	}

	return strconv.FormatInt(int64(result), 10)
}

func Solve() string {
	return compute(1_000_000)
}
